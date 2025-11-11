#include "assembler.h"

#include <ctype.h>
#include <execinfo.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "constants.h"
#include "cpu.h"
#include "helper.h"
#include "opcodes.h"
#include "tables.h"
#include "uthash.h"

// A couple of notes about formatting for the assembler:
// ^label^ means upper 16 bits of address
// .label. means lower 16 bits of address
// @label@ means 26-bit address (for jump instructions)
// !label! means 16-bit relative address (for branch instructions)

// Represented in templates by number
typedef enum DataType {
  REGISTER,         // 0
  LABEL,            // 1, only for jump instructions
  STRING,           // 2, only for ascii directive: uses quotes
  INT5,             // 3
  INT16,            // 4
  INT32,            // 5
  IMMEDIATE,        // Could be either a label, INT16, or INT32 (6)
  SHORT_IMMEDIATE,  // Could be either a label or INT16 (7)
  INT8LIST,         // 8, only for .byte directive
  INT16LIST,        // 9, only for .half directive
  INT32LIST         // ":" (next ascii char after 9) , only for .word directive
} DataType;

typedef struct LineData {
  char* label;
  char* line;
} LineData;

struct Assembler;

typedef struct Directive {
  char* name;
  char* template;
  void (*handler)(struct Assembler* asmblr, char** args, int arg_count);
  UT_hash_handle hh;
} Directive;

typedef struct Command {
  char* name;
  char* template;
  void (*handler)(struct Assembler* asmblr, char** args, int arg_count);
  UT_hash_handle hh;
} Command;

typedef struct Assembler {
  LineData* lines;         // Array of lines from the source file
  uint16_t line_count;     // Number of lines read
  bool is_data;            // 1 = .data section, 0 = .text section
  bool error;              // Whether an error has occurred
  StrToUint32* labels;     // Hash table of labels to addresses
  StrToUint32* reg_map;    // Hash table of register names to numbers
  Directive* dir_map;      // Hash table of directives
  Command* cmd_map;        // Hash table of commands
  LineData* intmd;         // Plain-text machine code with labels
  uint16_t intmd_index;    // Current index in machine code output
  char** storage_instr;    // Plain-text machine code (kernel storage)
  uint16_t storage_index;  // Current index in storage instructions
  uint8_t align;           // Current alignment for .data section
  uint32_t data_address;   // Current data segment address
} Assembler;

void error(Assembler* asmblr, uint16_t line_number) {
  char* line = asmblr->lines[line_number].line;
  char* label = asmblr->lines[line_number].label;

  // Add : space after label if exists
  if (label) {
    fprintf(stderr, "Syntax error on line %d: '%s: %s'\n", line_number + 1,
            label, line);
  } else {
    fprintf(stderr, "Syntax error on line %d: '%s'\n", line_number + 1, line);
  }

  asmblr->error = true;
}

void add_dir(Assembler* asmblr, const char* name, const char* template,
             void (*handler)(Assembler* asmblr, char** args, int arg_count)) {
  Directive* r = malloc(sizeof(Directive));
  r->name = strdup(name);
  r->template = strdup(template);
  r->handler = handler;
  HASH_ADD_KEYPTR(hh, asmblr->dir_map, r->name, strlen(r->name), r);
}

void add_cmd(Assembler* asmblr, const char* name, const char* template,
             void (*handler)(Assembler* asmblr, char** args, int arg_count)) {
  Command* r = malloc(sizeof(Command));
  r->name = strdup(name);
  r->template = strdup(template);
  r->handler = handler;
  HASH_ADD_KEYPTR(hh, asmblr->cmd_map, r->name, strlen(r->name), r);
}

Directive* get_dir(Assembler* asmblr, const char* name) {
  Directive* r;
  HASH_FIND_STR(asmblr->dir_map, name, r);
  return r;
}

Command* get_cmd(Assembler* asmblr, const char* name) {
  Command* r;
  HASH_FIND_STR(asmblr->cmd_map, name, r);
  return r;
}

bool match_register(Assembler* asmblr, char* line) {
  return get_value(&asmblr->reg_map, line) != 0xFFFFFFFF;
}

bool match_label(Assembler* asmblr, char* line) {
  return get_value(&asmblr->labels, line) != 0xFFFFFFFF;
}

bool match_string(char* line) {
  char* unescaped = unescape_c_string(line);
  if (unescaped == NULL) {
    return false;
  } else {
    free(unescaped);
    return true;
  }
}

bool match_int(char* line, int bits) { return parse_int(line, bits, NULL); }

void add_r_instruction(Assembler* asmblr, char* label, char* rs, char* rt,
                       char* rd, char* shamt, uint8_t funct) {
  uint8_t rs_num = (uint8_t)get_value(&asmblr->reg_map, rs);
  uint8_t rt_num = (uint8_t)get_value(&asmblr->reg_map, rt);
  uint8_t rd_num = (uint8_t)get_value(&asmblr->reg_map, rd);
  uint32_t shamt_num;
  parse_int(shamt, 5, &shamt_num);

  uint32_t instr = 0;
  instr |= ((uint32_t)(rs_num & 0x1F)) << 21;
  instr |= ((uint32_t)(rt_num & 0x1F)) << 16;
  instr |= ((uint32_t)(rd_num & 0x1F)) << 11;
  instr |= ((uint32_t)(shamt_num & 0x1F)) << 6;
  instr |= ((uint32_t)(funct & 0x3F));

  // Convert to string and store in intmd
  char* instr_str = int_to_binary(instr, 32);
  asmblr->intmd[asmblr->intmd_index++].line = instr_str;
  if (label != NULL)
    asmblr->intmd[asmblr->intmd_index - 1].label = strdup(label);
}

char* get_i_instruction(Assembler* asmblr, char* rs, char* rt,
                        uint16_t immediate, uint8_t opcode) {
  uint8_t rs_num = (uint8_t)get_value(&asmblr->reg_map, rs);
  uint8_t rt_num = (uint8_t)get_value(&asmblr->reg_map, rt);

  // Create instruction
  uint32_t instr = 0;
  instr |= ((uint32_t)(opcode & 0x3F)) << 26;
  instr |= ((uint32_t)(rs_num & 0x1F)) << 21;
  instr |= ((uint32_t)(rt_num & 0x1F)) << 16;
  instr |= (uint32_t)(immediate & 0xFFFF);

  char* instr_str = int_to_binary(instr, 32);
  return instr_str;
}

void add_i_instruction(Assembler* asmblr, char* label, char* rs, char* rt,
                       char* immediate, uint8_t opcode) {
  uint32_t imm_num;
  parse_int(immediate, 16, &imm_num);

  char* instr_str =
      get_i_instruction(asmblr, rs, rt, (uint16_t)imm_num, opcode);
  asmblr->intmd[asmblr->intmd_index++].line = instr_str;
  if (label != NULL)
    asmblr->intmd[asmblr->intmd_index - 1].label = strdup(label);
}

void add_i_instruction_int(Assembler* asmblr, char* label, char* rs, char* rt,
                           int16_t immediate, uint8_t opcode) {
  uint32_t imm_num = (uint32_t)((uint16_t)immediate);

  char* instr_str =
      get_i_instruction(asmblr, rs, rt, (uint16_t)imm_num, opcode);
  asmblr->intmd[asmblr->intmd_index++].line = instr_str;
  if (label != NULL)
    asmblr->intmd[asmblr->intmd_index - 1].label = strdup(label);
}

void add_i_label_instruction(Assembler* asmblr, char* label, char* rs, char* rt,
                             char* immediate, uint8_t opcode) {
  // Keep imm as a string for now
  size_t imm_len = strlen(immediate);
  // Enough space for opcode, rs, rt, immediate, and null terminator
  char* instr_str = malloc(17 + imm_len);

  // Build instruction string
  char* opcode_str = int_to_binary((uint32_t)opcode, 6);
  char* rs_str = int_to_binary((uint32_t)get_value(&asmblr->reg_map, rs), 5);
  char* rt_str = int_to_binary((uint32_t)get_value(&asmblr->reg_map, rt), 5);
  snprintf(instr_str, 17 + imm_len, "%s%s%s%s", opcode_str, rs_str, rt_str,
           immediate);
  free(opcode_str);
  free(rs_str);
  free(rt_str);

  asmblr->intmd[asmblr->intmd_index++].line = instr_str;
  if (label != NULL)
    asmblr->intmd[asmblr->intmd_index - 1].label = strdup(label);
}

void add_j_instruction(Assembler* asmblr, char* line_label, char* label,
                       uint8_t opcode) {
  // Keep label as a string for now; surround label with special char (@)
  size_t label_len = strlen(label);
  // Enough space for opcode, label, two @s, and null terminator
  char* instr_str = malloc(9 + label_len);

  // Build instruction string
  char* opcode_str = int_to_binary((uint32_t)opcode, 6);
  snprintf(instr_str, 9 + label_len, "%s@%s@", opcode_str, label);
  free(opcode_str);

  asmblr->intmd[asmblr->intmd_index++].line = instr_str;
  if (line_label != NULL)
    asmblr->intmd[asmblr->intmd_index - 1].label = strdup(line_label);
}

void store_string(Assembler* asmblr, const char* str, const char* label,
                  bool null_terminated) {
  // Assembly form
  // lui $k0, upper_addr
  // ori $k0, $k0, lower_addr
  // addi $k1, $zero, <char1>
  // sb $k1, 0($k0)
  // addi $k0, $k0, 1
  // ... (until string is word-aligned)
  // lui $k1, upper_word
  // ori $k1, $k1, lower_word
  // sw $k1, 0($k0)
  // addi $k0, $k0, 4
  // ... (for each word)

  size_t len = strlen(str);
  size_t total_len = null_terminated ? len + 1 : len;

  uint32_t align_mask = (1U << asmblr->align) - 1;
  if (asmblr->data_address > UINT32_MAX - align_mask)
    return error(asmblr, asmblr->intmd_index);

  // Align to current assembler’s alignment
  asmblr->data_address = (asmblr->data_address + align_mask) & ~align_mask;

  if (asmblr->data_address > UINT32_MAX - total_len)
    return error(asmblr, asmblr->intmd_index);

  // Not exact, but safe upper bound on instruction count
  if (asmblr->storage_index + 3 * total_len + 2 >= PROGRAM_SIZE)
    return error(asmblr, asmblr->intmd_index);

  // Label
  if (label != NULL) {
    StrToUint32* label_entry;
    HASH_FIND_STR(asmblr->labels, label, label_entry);
    label_entry->value = asmblr->data_address;
  }

  uint16_t upper_addr = (asmblr->data_address >> 16) & 0xFFFF;
  uint16_t lower_addr = asmblr->data_address & 0xFFFF;

  asmblr->storage_instr[asmblr->storage_index++] =
      get_i_instruction(asmblr, "$zero", "$k0", upper_addr, LUI_OP);
  asmblr->storage_instr[asmblr->storage_index++] =
      get_i_instruction(asmblr, "$k0", "$k0", lower_addr, ORI_OP);

  size_t i = 0;

  // Handle unaligned prefix bytes
  while ((asmblr->data_address & 3) && i < total_len) {
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$zero", "$k1", (uint16_t)str[i], ADDI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k1", 0, SB_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k0", 1, ADDI_OP);
    asmblr->data_address++;
    i++;
  }

  // Handle bulk aligned words
  for (; i + 4 <= total_len; i += 4) {
    uint32_t word = 0;
    for (int b = 0; b < 4; b++)
      word |= (uint8_t)str[i + b] << (8 * (3 - b));  // big-endian pack

    uint16_t hi = (word >> 16) & 0xFFFF;
    uint16_t lo = word & 0xFFFF;

    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k1", 0, SW_OP);

    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k0", 4, ADDI_OP);
    asmblr->data_address += 4;
  }

  // After bulk-aligned words
  if (i < total_len) {
    uint32_t word = 0;
    int remaining = total_len - i;

    for (int b = 0; b < 4; b++) {
      uint8_t c = (b < remaining) ? (uint8_t)str[i + b] : 0;
      word |= (uint32_t)c << (8 * (3 - b));  // big-endian pack
    }

    uint16_t hi = (word >> 16) & 0xFFFF;
    uint16_t lo = word & 0xFFFF;

    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k1", 0, SW_OP);

    asmblr->data_address += remaining;
  }
}

void store_space(Assembler* asmblr, char* label, uint32_t size) {
  uint32_t mask = (1U << asmblr->align) - 1;
  asmblr->data_address = (asmblr->data_address + mask) & ~mask;

  // store label if given
  if (label != NULL) {
    StrToUint32* entry;
    HASH_FIND_STR(asmblr->labels, label, entry);
    if (entry) entry->value = asmblr->data_address;
  }

  // reserve the space
  if (asmblr->data_address > UINT32_MAX - size) {
    error(asmblr, asmblr->intmd_index);
    return;
  }
  asmblr->data_address += size;
}

static inline uint8_t extract_be_byte(uint32_t val, int bytes, int index) {
  return (val >> (8 * (bytes - 1 - index))) & 0xFFu;
}

void store_data(Assembler* asmblr, char* label, char** args, int arg_count,
                int bytes) {
  // Respect assembler-level alignment (e.g., .align directive)
  uint32_t mask = (1U << asmblr->align) - 1;
  asmblr->data_address = (asmblr->data_address + mask) & ~mask;

  // Store label if present
  if (label != NULL) {
    StrToUint32* entry;
    HASH_FIND_STR(asmblr->labels, label, entry);
    if (entry) entry->value = asmblr->data_address;
  }

  int cur = 0;  // index into args[]
  int byte_offset =
      0;  // how many bytes of current arg we've consumed (0..bytes-1)
  uint32_t cur_val =
      0;  // parsed numeric value of current arg (valid when cur < arg_count)
  bool have_cur_val = false;

  // Load $k0 with starting address
  uint16_t upper_addr = (asmblr->data_address >> 16) & 0xFFFF;
  uint16_t lower_addr = asmblr->data_address & 0xFFFF;
  asmblr->storage_instr[asmblr->storage_index++] =
      get_i_instruction(asmblr, "$zero", "$k0", upper_addr, LUI_OP);
  asmblr->storage_instr[asmblr->storage_index++] =
      get_i_instruction(asmblr, "$k0", "$k0", lower_addr, ORI_OP);

  // Stage 1: prefix bytes until addr is word-aligned
  while (cur < arg_count && (asmblr->data_address & 3) != 0) {
    if (!have_cur_val) {
      if (!parse_int(args[cur], bytes * 8, &cur_val)) {
        error(asmblr, asmblr->intmd_index);
        return;
      }
      have_cur_val = true;
      byte_offset = 0;
    }

    // extract next byte (big-endian within element)
    uint8_t b = extract_be_byte(cur_val, bytes, byte_offset);

    // emit: addi $k1, $zero, b ; sb $k1, 0($k0); addi $k0, $k0, 1
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$zero", "$k1", (uint16_t)b, ADDI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k1", 0, SB_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k0", 1, ADDI_OP);

    byte_offset++;
    asmblr->data_address += 1;

    if (byte_offset >= bytes) {
      cur++;
      have_cur_val = false;
    }
  }

  // Stage 2: pack full words (this loop also emits a final partial-word padded
  // with zeros)
  while (cur < arg_count) {
    uint32_t word = 0;
    int packed = 0;  // bytes packed into `word` so far (0..4)

    while (packed < 4 && cur < arg_count) {
      if (!have_cur_val) {
        if (!parse_int(args[cur], bytes * 8, &cur_val)) {
          error(asmblr, asmblr->intmd_index);
          return;
        }
        have_cur_val = true;
        byte_offset = 0;
      }

      int remaining_in_arg = bytes - byte_offset;
      int space_in_word = 4 - packed;
      int take =
          remaining_in_arg < space_in_word ? remaining_in_arg : space_in_word;

      // copy `take` bytes from current arg into `word` (big-endian)
      for (int k = 0; k < take; ++k) {
        uint8_t bb = extract_be_byte(cur_val, bytes, byte_offset + k);
        int shift = 8 * (4 - packed - 1 - k);  // position in word (big-endian)
        word |= (uint32_t)bb << shift;
      }

      packed += take;
      byte_offset += take;

      if (byte_offset >= bytes) {
        cur++;
        have_cur_val = false;
      }
    }

    // Emit LUI/ORI/SW for `word`
    uint16_t hi = (word >> 16) & 0xFFFFu;
    uint16_t lo = word & 0xFFFFu;
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP);
    asmblr->storage_instr[asmblr->storage_index++] =
        get_i_instruction(asmblr, "$k0", "$k1", 0, SW_OP);
    // Increment $k0 by 4 bytes (except last time)
    if (cur < arg_count) {
      asmblr->storage_instr[asmblr->storage_index++] =
          get_i_instruction(asmblr, "$k0", "$k0", 4, ADDI_OP);
    }

    // Incremenet addr by number of bytes actually packed
    asmblr->data_address += packed;
  }
}

char* get_upper(const char* label) {
  size_t label_len = strlen(label);
  char* upper_label = malloc(label_len + 3);  // for ^, ^, and null terminator
  snprintf(upper_label, label_len + 3, "^%s^", label);
  return upper_label;
}

char* get_lower(const char* label) {
  size_t label_len = strlen(label);
  char* lower_label = malloc(label_len + 3);  // for <, >, and null terminator
  snprintf(lower_label, label_len + 3, ".%s.", label);
  return lower_label;
}

void pseudo_immediate(Assembler* asmblr, char* rs, char* rt, char* imm,
                      int opcode, int lui_op, int ori_op, char* lui_src,
                      char* line_label) {
  if (match_label(asmblr, imm)) {
    // Immediate is a label: expand to pseudo-instruction sequence

    // Split label into upper and lower 16-bit parts
    char* upper_label = get_upper(imm);
    char* lower_label = get_lower(imm);

    // LUI: load upper 16 bits of label into $at
    add_i_label_instruction(asmblr, line_label, lui_src, "$at", upper_label,
                            lui_op);
    // ORI: add lower 16 bits to $at
    add_i_label_instruction(asmblr, NULL, "$at", "$at", lower_label, ori_op);
    // ADDU: combine with rs into destination rt
    add_r_instruction(asmblr, NULL, rs, "$at", rt, "0", ADDU_OP);

    free(upper_label);
    free(lower_label);

  } else if (!match_int(imm, 15) && match_int(imm, 32)) {
    // Immediate is larger than 15 bits: use pseudo-instruction
    uint32_t val;
    parse_int(imm, 32, &val);
    uint16_t upper_imm = (val >> 16) & 0xFFFF;  // upper 16 bits
    uint16_t lower_imm = val & 0xFFFF;          // lower 16 bits

    // LUI: load upper 16 bits into $at
    add_i_instruction_int(asmblr, line_label, lui_src, "$at", upper_imm,
                          lui_op);
    // ORI: add lower 16 bits to $at
    add_i_instruction_int(asmblr, NULL, "$at", "$at", lower_imm, ori_op);
    // ADDU: combine with rs into rt
    add_r_instruction(asmblr, NULL, rs, "$at", rt, "0", ADDU_OP);

  } else {
    // Immediate fits in 16 bits → normal I-type instruction
    add_i_instruction(asmblr, line_label, rs, rt, imm, opcode);
  }
}

static void _branch(Assembler* asmblr, char* rs, char* rt, char* imm,
                    int opcode, char* line_label) {
  // Syntax: branch rs, rt, label/immediate
  // Add ! around label to indicate relative address
  if (match_label(asmblr, imm)) {
    char* rel_label = malloc(strlen(imm) + 3);
    snprintf(rel_label, strlen(imm) + 3, "!%s!", imm);
    add_i_label_instruction(asmblr, line_label, rs, rt, rel_label, opcode);
    free(rel_label);
  } else {
    add_i_instruction(asmblr, line_label, rs, rt, imm, opcode);
  }
}

static void _pseudo_branch(Assembler* asmblr, char* rs, char* rt, char* imm,
                           int branch_opcode, int cmp_op, char* line_label) {
  // Syntax for pseudo-instruction:
  // slt/ sltu $at, rs, rt
  // beq/bne $at, $zero, label/immediate
  add_r_instruction(asmblr, NULL, rs, rt, "$at", "0", cmp_op);
  _branch(asmblr, "$at", "$zero", imm, branch_opcode, line_label);
}

static void DATA_dir(Assembler* asmblr, char** args, int arg_count) {
  (void)args;
  (void)arg_count;
  asmblr->is_data = true;
}

static void TEXT_dir(Assembler* asmblr, char** args, int arg_count) {
  (void)args;
  (void)arg_count;
  asmblr->is_data = false;
}

static void ALIGN_dir(Assembler* asmblr, char** args, int arg_count) {
  (void)arg_count;
  uint32_t align_val;
  parse_int(args[0], 5, &align_val);
  asmblr->align = (uint8_t)align_val;
}

static void ASCII_dir(Assembler* asmblr, char** args, int arg_count) {
  char* str = unescape_c_string(args[0]);
  store_string(asmblr, str, args[arg_count - 1], false);
  free(str);
}

static void ASCIIZ_dir(Assembler* asmblr, char** args, int arg_count) {
  char* str = unescape_c_string(args[0]);
  store_string(asmblr, str, args[arg_count - 1], true);
  free(str);
}

static void SPACE_dir(Assembler* asmblr, char** args, int arg_count) {
  uint32_t size;
  parse_int(args[0], 32, &size);
  store_space(asmblr, args[arg_count - 1], size);
}

static void WORD_dir(Assembler* asmblr, char** args, int arg_count) {
  store_data(asmblr, args[arg_count - 1], args, arg_count - 1, 4);
}

static void HALF_dir(Assembler* asmblr, char** args, int arg_count) {
  store_data(asmblr, args[arg_count - 1], args, arg_count - 1, 2);
}

static void BYTE_dir(Assembler* asmblr, char** args, int arg_count) {
  store_data(asmblr, args[arg_count - 1], args, arg_count - 1, 1);
}

static void ADD(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: add rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    ADD_OP);
}

static void ADDU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: addu rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    ADDU_OP);
}

static void ADDI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: addi rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ADDI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void ADDIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: addiu rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ADDIU_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void AND(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: and rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    AND_OP);
}

static void ANDI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: andi rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ANDI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void DIV(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: div $rs, $rt
  add_r_instruction(asmblr, args[arg_count - 1], args[0], args[1], "0", "0",
                    DIV_OP);
}

static void DIVU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: divu $rs, $rt
  add_r_instruction(asmblr, args[arg_count - 1], args[0], args[1], "0", "0",
                    DIVU_OP);
}

static void MULT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mult $rs, $rt
  add_r_instruction(asmblr, args[arg_count - 1], args[0], args[1], "0", "0",
                    MULT_OP);
}

static void MULTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: multu $rs, $rt
  add_r_instruction(asmblr, args[arg_count - 1], args[0], args[1], "0", "0",
                    MULTU_OP);
}

static void NOR(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: nor rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    NOR_OP);
}

static void OR(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: or rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    OR_OP);
}

static void ORI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: ori rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ORI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void SLL(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sll rd, rt, shamt
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", args[1], args[0],
                    args[2], SLL_OP);
}

static void SLLV(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sllv rd, rt, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[2], args[1], args[0], "0",
                    SLLV_OP);
}

static void SRA(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sra rd, rt, shamt
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", args[1], args[0],
                    args[2], SRA_OP);
}

static void SRAV(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: srav rd, rt, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[2], args[1], args[0], "0",
                    SRAV_OP);
}

static void SRL(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: srl rd, rt, shamt
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", args[1], args[0],
                    args[2], SRL_OP);
}

static void SRLV(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: srlv rd, rt, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[2], args[1], args[0], "0",
                    SRLV_OP);
}

static void SUB(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sub rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    SUB_OP);
}

static void SUBU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: subu rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    SUBU_OP);
}

static void XOR(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: xor rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    XOR_OP);
}

static void XORI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: xori rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], XORI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void LUI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lui rt, immediate
  add_i_instruction(asmblr, args[arg_count - 1], "$zero", args[0], args[1],
                    LUI_OP);
}

static void SLT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: slt rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    SLT_OP);
}

static void SLTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sltu rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    SLTU_OP);
}

static void SLTI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: slti rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], SLTI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void SLTIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sltiu rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], SLTIU_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1]);
}

static void B(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: b label/immediate -> beq $zero, $zero, label/immediate
  _branch(asmblr, "$zero", "$zero", args[0], BEQ_OP, args[arg_count - 1]);
}

static void BEQ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: beq rs, rt, label/immediate
  _branch(asmblr, args[0], args[1], args[2], BEQ_OP, args[arg_count - 1]);
}

static void BNE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bne rs, rt, label/immediate
  _branch(asmblr, args[0], args[1], args[2], BNE_OP, args[arg_count - 1]);
}

static void BEQZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: beqz rs, label/immediate
  _branch(asmblr, args[0], "$zero", args[1], BEQ_OP, args[arg_count - 1]);
}

static void BNEZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bnez rs, label/immediate
  _branch(asmblr, args[0], "$zero", args[1], BNE_OP, args[arg_count - 1]);
}

static void BLTZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bltz rs, label/immediate
  _branch(asmblr, args[0], "$0", args[1], BLTZ_or_BGEZ_OP, args[arg_count - 1]);
}

static void BGEZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgez rs, label/immediate
  _branch(asmblr, args[0], "$1", args[1], BLTZ_or_BGEZ_OP, args[arg_count - 1]);
}

static void BGTZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgtz rs, label/immediate
  _branch(asmblr, args[0], "$zero", args[1], BGTZ_OP, args[arg_count - 1]);
}

static void BLEZ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: blez rs, label/immediate
  _branch(asmblr, args[0], "$zero", args[1], BLEZ_OP, args[arg_count - 1]);
}

static void BLT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: blt rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BNE_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BGT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgt rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BNE_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BLE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: ble rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BEQ_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BGE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bge rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BEQ_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BLTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bltu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BNE_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BGTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgtu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BNE_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BLEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bleu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BEQ_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BGEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgeu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BEQ_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void SYSCALL(Assembler* asmblr, char** args, int arg_count) {
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", "$0", "0",
                    SYSCALL_OP);
}

static void J(Assembler* asmblr, char** args, int arg_count) {
  add_j_instruction(asmblr, args[arg_count - 1], args[0], J_OP);
}

static void JAL(Assembler* asmblr, char** args, int arg_count) {
  add_j_instruction(asmblr, args[arg_count - 1], args[0], JAL_OP);
}

static void JALR(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: jalr rd, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[1], "$zero", args[0], "0",
                    JALR_OP);
}

static void JR(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: jr rs
  add_r_instruction(asmblr, args[arg_count - 1], args[0], "$zero", "$zero", "0",
                    JR_OP);
}

static void SB(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sb rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], SB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void SH(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sh rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], SH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void SW(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sw rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], SW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LB(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lb rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LBU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lbu rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LBU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LH(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lh rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LHU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lhu rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LHU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LW(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lw rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void MFHI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mfhi rd
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFHI_OP);
}

static void MFLO(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mflo rd
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFLO_OP);
}

static void MTHI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mthi rs
  add_r_instruction(asmblr, args[arg_count - 1], args[0], "$0", "$0", "0",
                    MTHI_OP);
}

static void MTLO(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mtlo rs
  add_r_instruction(asmblr, args[arg_count - 1], args[0], "$0", "$0", "0",
                    MTLO_OP);
}

static void MOVE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: move rd, rs -> addu rd, rs, $zero
  add_r_instruction(asmblr, args[arg_count - 1], args[1], "$zero", args[0], "0",
                    ADDU_OP);
}

static void LI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: li rt, immediate -> addiu rt, $zero, immediate
  pseudo_immediate(asmblr, "$zero", args[0], args[1], ADDIU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void LA(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: la rt, label -> addiu rt, $zero, label
  pseudo_immediate(asmblr, "$zero", args[0], args[1], ADDIU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1]);
}

static void NOP(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: nop -> sll $zero, $zero, 0
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", "$zero", "0",
                    SLL_OP);
}

static void init_instructions(Assembler* asmblr) {
  add_dir(asmblr, "data", "", DATA_dir);
  add_dir(asmblr, "text", "", TEXT_dir);
  add_dir(asmblr, "align", "3", ALIGN_dir);
  add_dir(asmblr, "ascii", "2", ASCII_dir);
  add_dir(asmblr, "asciiz", "2", ASCIIZ_dir);
  add_dir(asmblr, "space", "4", SPACE_dir);
  add_dir(asmblr, "word", ":", WORD_dir);
  add_dir(asmblr, "half", "9", HALF_dir);
  add_dir(asmblr, "byte", "8", BYTE_dir);

  add_cmd(asmblr, "add", "0 0 0", ADD);
  add_cmd(asmblr, "addu", "0 0 0", ADDU);
  add_cmd(asmblr, "addi", "0 0 6", ADDI);
  add_cmd(asmblr, "addiu", "0 0 6", ADDIU);
  add_cmd(asmblr, "and", "0 0 0", AND);
  add_cmd(asmblr, "andi", "0 0 6", ANDI);
  add_cmd(asmblr, "div", "0 0", DIV);
  add_cmd(asmblr, "divu", "0 0", DIVU);
  add_cmd(asmblr, "mult", "0 0", MULT);
  add_cmd(asmblr, "multu", "0 0", MULTU);
  add_cmd(asmblr, "nor", "0 0 0", NOR);
  add_cmd(asmblr, "or", "0 0 0", OR);
  add_cmd(asmblr, "ori", "0 0 6", ORI);
  add_cmd(asmblr, "sll", "0 0 3", SLL);
  add_cmd(asmblr, "sllv", "0 0 0", SLLV);
  add_cmd(asmblr, "sra", "0 0 3", SRA);
  add_cmd(asmblr, "srav", "0 0 0", SRAV);
  add_cmd(asmblr, "srl", "0 0 3", SRL);
  add_cmd(asmblr, "srlv", "0 0 0", SRLV);
  add_cmd(asmblr, "sub", "0 0 0", SUB);
  add_cmd(asmblr, "subu", "0 0 0", SUBU);
  add_cmd(asmblr, "xor", "0 0 0", XOR);
  add_cmd(asmblr, "xori", "0 0 6", XORI);
  add_cmd(asmblr, "lui", "0 4", LUI);
  add_cmd(asmblr, "slt", "0 0 0", SLT);
  add_cmd(asmblr, "sltu", "0 0 0", SLTU);
  add_cmd(asmblr, "slti", "0 0 6", SLTI);
  add_cmd(asmblr, "sltiu", "0 0 6", SLTIU);

  add_cmd(asmblr, "b", "7", B);
  add_cmd(asmblr, "beq", "0 0 7", BEQ);
  add_cmd(asmblr, "bne", "0 0 7", BNE);
  add_cmd(asmblr, "beqz", "0 7", BEQZ);
  add_cmd(asmblr, "bnez", "0 7", BNEZ);
  add_cmd(asmblr, "bltz", "0 7", BLTZ);
  add_cmd(asmblr, "bgez", "0 7", BGEZ);
  add_cmd(asmblr, "bgtz", "0 7", BGTZ);
  add_cmd(asmblr, "blez", "0 7", BLEZ);
  add_cmd(asmblr, "blt", "0 0 7", BLT);
  add_cmd(asmblr, "bgt", "0 0 7", BGT);
  add_cmd(asmblr, "ble", "0 0 7", BLE);
  add_cmd(asmblr, "bge", "0 0 7", BGE);
  add_cmd(asmblr, "bltu", "0 0 7", BLTU);
  add_cmd(asmblr, "bgtu", "0 0 7", BGTU);
  add_cmd(asmblr, "bleu", "0 0 7", BLEU);
  add_cmd(asmblr, "bgeu", "0 0 7", BGEU);

  add_cmd(asmblr, "j", "1", J);
  add_cmd(asmblr, "jal", "1", JAL);
  add_cmd(asmblr, "jalr", "0 1", JALR);
  add_cmd(asmblr, "jr", "0", JR);
  add_cmd(asmblr, "syscall", "", SYSCALL);

  add_cmd(asmblr, "sb", "0 6(0)", SB);
  add_cmd(asmblr, "sh", "0 6(0)", SH);
  add_cmd(asmblr, "sw", "0 6(0)", SW);
  add_cmd(asmblr, "lb", "0 6(0)", LB);
  add_cmd(asmblr, "lbu", "0 6(0)", LBU);
  add_cmd(asmblr, "lh", "0 6(0)", LH);
  add_cmd(asmblr, "lhu", "0 6(0)", LHU);
  add_cmd(asmblr, "lw", "0 6(0)", LW);
  add_cmd(asmblr, "mfhi", "0", MFHI);
  add_cmd(asmblr, "mflo", "0", MFLO);
  add_cmd(asmblr, "mthi", "0", MTHI);
  add_cmd(asmblr, "mtlo", "0", MTLO);

  add_cmd(asmblr, "move", "0 0", MOVE);
  add_cmd(asmblr, "li", "0 6", LI);
  add_cmd(asmblr, "la", "0 6", LA);
  add_cmd(asmblr, "nop", "", NOP);
}

void strip_comments(Assembler* asmblr) {
  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    while (*line) {
      if (*line == '#') {
        *line = '\0';
        break;
      }
      line++;
    }
  }
}

void replace_commas_with_spaces(Assembler* asmblr) {
  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    for (char* p = line; *p; p++) {
      if (*p == '"' || *p == '\'') {
        // stop processing the rest of the line
        break;
      }
      if (*p == ',') *p = ' ';
    }
  }
}

void normalize_whitespace(Assembler* asmblr) {
  int dst_index = 0;

  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    char* src = line;
    char* dst = line;

    // Skip leading whitespace
    while (*src && isspace((unsigned char)*src)) src++;

    int space_pending = 0;
    bool in_string = false;
    char string_delim = '\0';

    while (*src) {
      if (in_string) {
        *dst++ = *src;
        if (*src == string_delim) in_string = false;
      } else {
        if (*src == '"' || *src == '\'') {
          // flush pending space before entering string
          if (space_pending && dst != line) {
            *dst++ = ' ';
            space_pending = 0;
          }
          in_string = true;
          string_delim = *src;
          *dst++ = *src;
        } else if (isspace((unsigned char)*src)) {
          space_pending = 1;
        } else {
          if (space_pending && dst != line) {
            *dst++ = ' ';
            space_pending = 0;
          }
          *dst++ = *src;
        }
      }
      src++;
    }

    // Trim trailing whitespace
    while (dst > line && isspace((unsigned char)*(dst - 1))) dst--;
    *dst = '\0';

    if (*line != '\0') {
      asmblr->lines[dst_index++] = asmblr->lines[i];
    } else {
      free(line);
    }
  }

  asmblr->line_count = dst_index;
}

// Assumes whitespace has been normalized
void merge_labels(Assembler* asmblr) {
  int dst_index = 0;

  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;

    // If line ends with ':', merge with the next line
    size_t len = strlen(line);
    if (len > 0 && line[len - 1] == ':' && i + 1 < asmblr->line_count) {
      char* next_line = asmblr->lines[i + 1].line;

      // Create merged line
      size_t new_len = len + 1 + strlen(next_line) + 1;
      char* merged = malloc(new_len);
      snprintf(merged, new_len, "%s %s", line, next_line);

      free(line);
      free(next_line);

      asmblr->lines[dst_index++].line = merged;

      i++;  // skip the next line, already merged
    } else {
      asmblr->lines[dst_index++].line = line;
    }
  }

  asmblr->line_count = dst_index;
}

// Assume labels have been merged
void normalize_colons(Assembler* asmblr) {
  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    size_t len = strlen(line);

    char* buf = malloc(len * 2 + 1);  // worst case: space after every colon
    char* dst = buf;

    for (size_t j = 0; j < len; j++) {
      *dst++ = line[j];

      // Add space after colon if immediately followed by non-space
      if (line[j] == ':' && j + 1 < len &&
          !isspace((unsigned char)line[j + 1])) {
        *dst++ = ' ';
      }
    }

    *dst = '\0';
    free(line);
    asmblr->lines[i].line = buf;
  }
}

void extract_labels(Assembler* asmblr) {
  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    char* colon = strchr(line, ':');
    if (colon) {
      // Everything before colon is the label
      size_t label_len = colon - line;
      char* label = malloc(label_len + 1);
      strncpy(label, line, label_len);
      label[label_len] = '\0';

      // Make sure label is valid, must start with letter or underscore
      // followed by letters, digits, underscores
      if (!(isalpha((unsigned char)label[0]) || label[0] == '_')) {
        error(asmblr, i);
      } else {
        for (size_t j = 1; j < label_len; j++) {
          if (!(isalnum((unsigned char)label[j]) || label[j] == '_')) {
            error(asmblr, i);
          }
        }
      }

      asmblr->lines[i].label = label;
      // Save label to table (address will be filled in later)
      // 0xFFFFFFFE indicates unknown address for now
      // 0xFFFFFFFF reserved for undefined labels
      add_value(&asmblr->labels, label, 0xFFFFFFFE);

      // Skip the colon and any space immediately after
      char* rest = colon + 1;
      while (*rest && isspace((unsigned char)*rest)) rest++;

      // Shift rest of line to line.line
      char* new_line = strdup(rest);
      free(asmblr->lines[i].line);
      asmblr->lines[i].line = new_line;
    } else {
      asmblr->lines[i].label = NULL;
    }
  }
}

// Assume labels have been extracted
void lowercase_instructions(Assembler* asmblr) {
  for (int i = 0; i < asmblr->line_count; i++) {
    char* line = asmblr->lines[i].line;
    char* p = line;

    // Lowercase until first space or end of line
    while (*p && !isspace((unsigned char)*p)) {
      *p = tolower((unsigned char)*p);
      p++;
    }
  }
}

char** parse_template(Assembler* asmblr, const char* template,
                      const char* label, const char* line, int* out_count) {
  // Check for null line, matches against empty template
  if (!line || *line == '\0') {
    if (template && *template != '\0') {
      return NULL;  // caller will treat as parse failure
    } else {
      // Return an args array with one element: the label (may be NULL).
      // This preserves the invariant: args[arg_count-1] is the label.
      char** args = malloc(sizeof(char*));
      if (!args) return NULL;
      if (label == NULL) {
        args[0] = NULL;
      } else {
        args[0] = strdup(label);
        if (!args[0]) {
          free(args);
          return NULL;
        }
      }
      if (out_count) *out_count = 1;
      return args;
    }
  }

  const char* t = template;
  const char* l = line;
  int capacity = 4;
  int count = 0;
  char** args = malloc(sizeof(char*) * capacity);
  if (!args) return NULL;

  while (*t) {
    if (isdigit((unsigned char)*t) || *t == ':') {
      int list_type = *t - '0';
      if (list_type == INT8LIST || list_type == INT16LIST ||
          list_type == INT32LIST) {
        // Handle list of integers
        t++;
        while (*l) {
          // Skip leading whitespace
          while (*l && isspace((unsigned char)*l)) l++;
          if (*l == '\0') break;

          const char* token_start = l;
          const char* token_end = token_start;
          while (*token_end && !isspace((unsigned char)*token_end)) {
            token_end++;
          }

          size_t len = token_end - token_start;
          char* value = malloc(len + 1);
          if (!value) {
            for (int i = 0; i < count; i++) free(args[i]);
            free(args);
            return NULL;
          }
          memcpy(value, token_start, len);
          value[len] = '\0';

          // validate token
          bool valid = false;
          switch (list_type) {
            case INT8LIST:
              valid = match_int(value, 8);
              break;
            case INT16LIST:
              valid = match_int(value, 16);
              break;
            case INT32LIST:
              valid = match_int(value, 32);
              break;
            default:
              valid = false;
          }

          if (!valid) {
            free(value);
            for (int i = 0; i < count; i++) free(args[i]);
            free(args);
            return NULL;
          }

          if (count >= capacity) {
            capacity *= 2;
            char** tmp = realloc(args, sizeof(char*) * capacity);
            if (!tmp) {
              free(value);
              for (int i = 0; i < count; i++) free(args[i]);
              free(args);
              return NULL;
            }
            args = tmp;
          }

          args[count++] = value;
          l = token_end;
        }
      } else {
        int type = *t - '0';
        const char* token_start = l;
        const char* token_end;

        if (type == STRING) {
          token_end = token_start + strlen(token_start);  // consume rest
        } else {
          token_end = token_start;
          while (*token_end) {
            char next_template = t[1];
            if (next_template && *token_end == next_template) break;
            if (isspace((unsigned char)*token_end)) break;
            token_end++;
          }
        }

        size_t len = token_end - token_start;
        char* value = malloc(len + 1);
        if (!value) {
          for (int i = 0; i < count; i++) free(args[i]);
          free(args);
          return NULL;
        }
        memcpy(value, token_start, len);
        value[len] = '\0';

        // validate token
        bool valid = false;
        switch (type) {
          case REGISTER:
            valid = match_register(asmblr, value);
            break;
          case LABEL:
            valid = match_label(asmblr, value);
            break;
          case STRING:
            valid = match_string(value);
            break;
          case INT5:
            valid = match_int(value, 5);
            break;
          case INT16:
            valid = match_int(value, 15);
            break;
          case INT32:
            valid = match_int(value, 32);
            break;
          case IMMEDIATE:
            valid = match_label(asmblr, value) || match_int(value, 32);
            break;
          case SHORT_IMMEDIATE:
            valid = match_label(asmblr, value) || match_int(value, 15);
            break;
          default:
            valid = false;
        }

        if (!valid) {
          free(value);
          for (int i = 0; i < count; i++) free(args[i]);
          free(args);
          return NULL;
        }

        if (count >= capacity) {
          capacity *= 2;
          char** tmp = realloc(args, sizeof(char*) * capacity);
          if (!tmp) {
            free(value);
            for (int i = 0; i < count; i++) free(args[i]);
            free(args);
            return NULL;
          }
          args = tmp;
        }

        args[count++] = value;
        l = token_end;
        t++;
      }
    } else {
      if (*t != *l) {
        for (int i = 0; i < count; i++) free(args[i]);
        free(args);
        return NULL;
      }
      t++;
      l++;
    }
  }

  if (*l != '\0') {
    for (int i = 0; i < count; i++) free(args[i]);
    free(args);
    return NULL;
  }

  // Add label to args
  char* label_copy = label == NULL ? NULL : strdup(label);
  if (count >= capacity) {
    capacity += 1;
    char** tmp = realloc(args, sizeof(char*) * capacity);
    if (!tmp) {
      free(label_copy);
      for (int i = 0; i < count; i++) free(args[i]);
      free(args);
      return NULL;
    }
    args = tmp;
  }
  args[count++] = label_copy;  // even if NULL

  if (out_count) *out_count = count;
  return args;
}

void compile_section(Assembler* asmblr, bool compile_data) {
  for (int i = 0; i < asmblr->line_count; i++) {
    const char* orig_line = asmblr->lines[i].line;
    char* line = strdup(orig_line);
    if (!line) {
      error(asmblr, i);
      return;
    }

    char* mnemonic = strtok(line, " ");
    char* rest = strtok(NULL, "");
    if (!mnemonic) {
      free(line);
      continue;
    }

    if (mnemonic[0] == '.') {
      Directive* d = get_dir(asmblr, mnemonic + 1);
      if (!d) {
        error(asmblr, i);
        free(line);
        return;
      }

      // Always handle .data and .text to switch context
      if (strcmp(d->name, "data") == 0 || strcmp(d->name, "text") == 0) {
        d->handler(asmblr, NULL, 0);
      }
      // Only compile if current section matches the pass
      else if (asmblr->is_data == compile_data) {
        int arg_count = 0;
        char** args = parse_template(asmblr, d->template,
                                     asmblr->lines[i].label, rest, &arg_count);
        if (!args) {
          error(asmblr, i);
          free(line);
          return;
        }
        d->handler(asmblr, args, arg_count);
        if (strcmp(d->name, "align") != 0) asmblr->align = 0;
        for (int j = 0; j < arg_count; j++) free(args[j]);
        free(args);
      }
    } else {
      // Only compile commands in text pass
      if (!compile_data && !asmblr->is_data) {
        Command* c = get_cmd(asmblr, mnemonic);
        if (!c) {
          error(asmblr, i);
          free(line);
          return;
        }

        int arg_count = 0;
        char** args = parse_template(asmblr, c->template,
                                     asmblr->lines[i].label, rest, &arg_count);
        if (!args) {
          error(asmblr, i);
          free(line);
          return;
        }

        c->handler(asmblr, args, arg_count);
        for (int j = 0; j < arg_count; j++) free(args[j]);
        free(args);
      }
    }

    free(line);
  }
}

void compile(Assembler* asmblr, uint32_t* machine_code, size_t* out_size) {
  (void)machine_code;

  strip_comments(asmblr);
  replace_commas_with_spaces(asmblr);
  normalize_whitespace(asmblr);
  merge_labels(asmblr);
  normalize_colons(asmblr);
  extract_labels(asmblr);
  lowercase_instructions(asmblr);

  // First pass: compile .data section
  compile_section(asmblr, true);
  if (asmblr->error) return;
  // Second pass: compile .text section
  compile_section(asmblr, false);
  if (asmblr->error) return;

  // Add j main to end of storage instructions
  // Enough space for opcode, @main@, and null terminator
  char* instr_str = malloc(13);
  char* opcode_str = int_to_binary((uint32_t)J_OP, 6);
  snprintf(instr_str, 13, "%s@%s@", opcode_str, "main");
  free(opcode_str);
  asmblr->storage_instr[asmblr->storage_index++] = instr_str;

  // Combine storage and intermediate instructions into single array
  size_t total_instructions = asmblr->storage_index + asmblr->intmd_index;
  if (total_instructions > PROGRAM_SIZE) {
    printf("HELLO\n");
    error(asmblr, asmblr->intmd_index);
    return;
  }
  LineData* all_instructions = malloc(sizeof(LineData) * total_instructions);

  for (size_t i = 0; i < asmblr->storage_index; i++) {
    all_instructions[i].line = strdup(asmblr->storage_instr[i]);
    all_instructions[i].label = NULL;
  }
  for (size_t j = 0; j < asmblr->intmd_index; j++) {
    LineData* src = &asmblr->intmd[j];
    LineData* dst = &all_instructions[asmblr->storage_index + j];
    dst->line = src->line ? strdup(src->line) : NULL;
    dst->label = src->label ? strdup(src->label) : NULL;
  }

  // Add labels to hash table (data labels have already been handled)
  for (size_t i = asmblr->storage_index; i < total_instructions; i++) {
    if (all_instructions[i].label != NULL) {
      StrToUint32* label_entry;
      HASH_FIND_STR(asmblr->labels, all_instructions[i].label, label_entry);
      if (label_entry) {
        // Byte address = TEXT_START_ADDRESS + instruction_index * 4
        label_entry->value = TEXT_START_ADDRESS + (uint32_t)(i * 4);
      }
    }
  }

  // Replace label/immediate placeholders in instructions
  for (size_t i = 0; i < total_instructions; i++) {
    char* line = all_instructions[i].line;
    char* new_line = malloc(50);  // 32 bits + null terminator
    char* dst = new_line;

    for (char* src = line; *src;) {
      if (*src == '^' || *src == '.' || *src == '@' || *src == '!') {
        char marker = *src++;
        char* start = src;
        while (*src && *src != marker) src++;
        size_t lablen = src - start;
        char label[lablen + 1];
        memcpy(label, start, lablen);
        label[lablen] = '\0';

        // Lookup label address
        StrToUint32* label_entry;
        HASH_FIND_STR(asmblr->labels, label, label_entry);
        if (!label_entry) {
          // Undefined label
          error(asmblr, (uint16_t)i);
          free(new_line);
          return;
        }
        uint32_t val = label_entry->value;

        uint32_t replacement_val = 0;

        switch (marker) {
          case '^':
            replacement_val = val >> 16;
            break;
          case '.':
            replacement_val = val & 0xFFFF;
            break;
          case '@':
            replacement_val = (val / 4) & 0x3FFFFFF;
            break;
          case '!': {
            uint32_t current_addr = TEXT_START_ADDRESS + i * 4;
            int offset = (val - (current_addr + 4)) / 4;
            replacement_val = (uint16_t)(offset & 0xFFFF);
            break;
          }
        }

        // Convert replacement to binary string offset by pipes for testing
        char* replacement =
            int_to_binary(replacement_val, (marker == '@') ? 26 : 16);

        size_t rep_len = strlen(replacement);
        memcpy(dst, replacement, rep_len);
        dst += rep_len;
        src++;  // skip closing marker
        free(replacement);
      } else {
        *dst++ = *src++;
      }
    }

    *dst = '\0';
    free(all_instructions[i].line);
    all_instructions[i].line = new_line;
  }

  // Convert all_instructions to machine code
  for (size_t i = 0; i < total_instructions; i++) {
    machine_code[i] = binary_to_int(all_instructions[i].line);
    free(all_instructions[i].line);
    if (all_instructions[i].label) free(all_instructions[i].label);
  }
  *out_size = total_instructions;
  free(all_instructions);

  // Get max label length
  size_t max_label_len = 0;
  for (int i = 0; i < asmblr->line_count; i++) {
    if (asmblr->lines[i].label) {
      size_t len = strlen(asmblr->lines[i].label);
      if (len > max_label_len) {
        max_label_len = len;
      }
    }
  }

  // Print asmblr lines for debugging
  printf("\nAssembled Lines:\n");
  for (int i = 0; i < asmblr->line_count; i++) {
    if (asmblr->lines[i].label) {
      printf("%-*s: %s\n", (int)max_label_len, asmblr->lines[i].label,
             asmblr->lines[i].line);
    } else {
      printf("%-*s  %s\n", (int)max_label_len, "", asmblr->lines[i].line);
    }
  }
}

uint32_t* assemble(char* filename, size_t* out_size) {
  // Load source file
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error: Could not open file %s\n", filename);
    return NULL;
  }

  // Initialize assembler
  Assembler* asmblr = (Assembler*)malloc(sizeof(Assembler));
  asmblr->lines = calloc(LINES, sizeof(LineData));
  asmblr->intmd = calloc(PROGRAM_SIZE, sizeof(LineData));
  asmblr->storage_instr = calloc(PROGRAM_SIZE, sizeof(char*));
  asmblr->is_data = false;
  asmblr->error = false;
  asmblr->labels = NULL;
  asmblr->reg_map = NULL;
  asmblr->dir_map = NULL;
  asmblr->cmd_map = NULL;
  asmblr->intmd_index = 0;
  asmblr->storage_index = 0;
  asmblr->line_count = 0;
  asmblr->align = 0;
  asmblr->data_address = DATA_START_ADDRESS;
  uint32_t* machine_code = calloc(PROGRAM_SIZE, sizeof(uint32_t));

  init_registers(&asmblr->reg_map);
  init_instructions(asmblr);

  // Read lines from file
  char* line = NULL;
  size_t len = 0;
  ssize_t read;
  uint16_t line_number = 0;

  while ((read = getline(&line, &len, file)) != -1) {
    // Strip newline character
    if (read > 0 && line[read - 1] == '\n') {
      line[read - 1] = '\0';
    }
    asmblr->lines[line_number].line = strdup(line);
    line_number++;
  }
  asmblr->line_count = line_number;
  free(line);
  fclose(file);

  // Construct machine code
  compile(asmblr, machine_code, out_size);

  // Free assembler except machine code
  for (int i = 0; i < asmblr->line_count; i++) {
    free(asmblr->lines[i].line);
    if (asmblr->lines[i].label) free(asmblr->lines[i].label);
  }
  free(asmblr->lines);

  for (int i = 0; i < asmblr->intmd_index; i++) {
    free(asmblr->intmd[i].line);
    if (asmblr->intmd[i].label) free(asmblr->intmd[i].label);
  }
  free(asmblr->intmd);

  for (int i = 0; i < asmblr->storage_index; i++) {
    free(asmblr->storage_instr[i]);
  }
  free(asmblr->storage_instr);

  // Free directives/commands
  Directive *dir_entry, *dir_tmp;
  HASH_ITER(hh, asmblr->dir_map, dir_entry, dir_tmp) {
    HASH_DEL(asmblr->dir_map, dir_entry);
    free(dir_entry->name);
    free(dir_entry->template);
    free(dir_entry);
  }
  Command *cmd_entry, *cmd_tmp;
  HASH_ITER(hh, asmblr->cmd_map, cmd_entry, cmd_tmp) {
    HASH_DEL(asmblr->cmd_map, cmd_entry);
    free(cmd_entry->name);
    free(cmd_entry->template);
    free(cmd_entry);
  }

  StrToUint32 *entry, *tmp;
  HASH_ITER(hh, asmblr->labels, entry, tmp) {
    HASH_DEL(asmblr->labels, entry);
    free(entry->key);
    free(entry);
  }
  HASH_ITER(hh, asmblr->reg_map, entry, tmp) {
    HASH_DEL(asmblr->reg_map, entry);
    free(entry->key);
    free(entry);
  }

  if (asmblr->error) {
    free(machine_code);
    free(asmblr);
    return NULL;
  }
  free(asmblr);

  return machine_code;
}
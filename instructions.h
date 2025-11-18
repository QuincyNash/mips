#ifndef _INSTRUCTIONS_H_
#define _INSTRUCTIONS_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "assembler.h"
#include "opcodes.h"
#include "uthash.h"

void error(Assembler* asmblr, int line_num) {
  (void)line_num;
  if (line_num >= 0) {
    fprintf(stderr, "Syntax Error on line %d\n", line_num + 1);
  } else {
    fprintf(stderr, "No main function defined\n");
  }

  asmblr->error = true;
}

Instruction* get_dir(Assembler* asmblr, const char* name) {
  Instruction* r;
  HASH_FIND_STR(asmblr->dir_map, name, r);
  return r;
}

Instruction* get_cmd(Assembler* asmblr, const char* name) {
  Instruction* r;
  HASH_FIND_STR(asmblr->cmd_map, name, r);
  return r;
}

void add_dir(Assembler* asmblr, const char* name, const char* tmpl,
             void (*handler)(Assembler* asmblr, char** args, int arg_count)) {
  // Check if already exists
  Instruction* existing = get_dir(asmblr, name);
  if (existing != NULL) {
    // Add new template to existing instruction
    TemplateExec te;
    te.tmpl = strdup(tmpl);
    te.handler = handler;
    vec_push(&existing->tmpls, te);
    existing->tmpl_count++;
  } else {
    // Create dynamic array of strings (tmpls)
    Instruction* r = (Instruction*)malloc(sizeof(Instruction));
    vec_init(&r->tmpls);

    TemplateExec te;
    te.tmpl = strdup(tmpl);
    te.handler = handler;
    vec_push(&r->tmpls, te);
    r->name = strdup(name);
    r->tmpl_count = 1;
    HASH_ADD_KEYPTR(hh, asmblr->dir_map, r->name, strlen(r->name), r);
  }
}

void add_cmd(Assembler* asmblr, const char* name, const char* tmpl,
             void (*handler)(Assembler* asmblr, char** args, int arg_count)) {
  // Check if already exists
  Instruction* existing = get_cmd(asmblr, name);
  if (existing != NULL) {
    // Add new template to existing instruction
    TemplateExec te;
    te.tmpl = strdup(tmpl);
    te.handler = handler;
    vec_push(&existing->tmpls, te);
    existing->tmpl_count++;
  } else {
    // Create dynamic array of strings (tmpls)
    Instruction* r = (Instruction*)malloc(sizeof(Instruction));
    vec_init(&r->tmpls);

    TemplateExec te;
    te.tmpl = strdup(tmpl);
    te.handler = handler;
    vec_push(&r->tmpls, te);
    r->name = strdup(name);
    r->tmpl_count = 1;
    HASH_ADD_KEYPTR(hh, asmblr->cmd_map, r->name, strlen(r->name), r);
  }
}

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

  LineData ld = {.label = label ? strdup(label) : NULL, .line = instr_str};
  vec_push(&asmblr->intmd, ld);
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

  LineData ld = {.label = label ? strdup(label) : NULL, .line = instr_str};
  vec_push(&asmblr->intmd, ld);
}

void add_i_instruction_int(Assembler* asmblr, char* label, char* rs, char* rt,
                           int16_t immediate, uint8_t opcode) {
  uint32_t imm_num = (uint32_t)((uint16_t)immediate);

  char* instr_str =
      get_i_instruction(asmblr, rs, rt, (uint16_t)imm_num, opcode);

  LineData ld = {.label = label ? strdup(label) : NULL, .line = instr_str};
  vec_push(&asmblr->intmd, ld);
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

  LineData ld = {.label = label ? strdup(label) : NULL, .line = instr_str};
  vec_push(&asmblr->intmd, ld);
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

  LineData ld = {.label = line_label ? strdup(line_label) : NULL,
                 .line = instr_str};
  vec_push(&asmblr->intmd, ld);
}

void store_string(Assembler* asmblr, const char* str, const char* label,
                  bool null_terminated) {
  // Assembly form
  // lui $k0, upper_addr
  // ori $k0, $k0, lower_addr
  // Store bytes one at a time using sb until aligned
  // Then store words using sw
  // Finally store any remaining bytes with final sw

  size_t len = strlen(str);
  size_t total_len = null_terminated ? len + 1 : len;

  uint32_t align_mask = (1U << asmblr->align) - 1;

  // Align to current assembler’s alignment
  asmblr->data_address = (asmblr->data_address + align_mask) & ~align_mask;

  // Label
  if (label != NULL) {
    StrToUint32* label_entry;
    HASH_FIND_STR(asmblr->labels, label, label_entry);
    label_entry->value = asmblr->data_address;
  }

  uint16_t upper_addr = (asmblr->data_address >> 16) & 0xFFFF;
  uint16_t lower_addr = asmblr->data_address & 0xFFFF;
  uint16_t curr_shift = 0;

  vec_push(&asmblr->storage_instr,
           get_i_instruction(asmblr, "$zero", "$k0", upper_addr, LUI_OP));
  vec_push(&asmblr->storage_instr,
           get_i_instruction(asmblr, "$k0", "$k0", lower_addr, ORI_OP));

  size_t i = 0;

  // Handle unaligned prefix bytes
  while ((asmblr->data_address & 3) && i < total_len) {
    vec_push(
        &asmblr->storage_instr,
        get_i_instruction(asmblr, "$zero", "$k1", (uint16_t)str[i], ADDI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k0", "$k1", curr_shift, SB_OP));
    curr_shift++;
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

    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k0", "$k1", curr_shift, SW_OP));
    curr_shift += 4;
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

    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k0", "$k1", curr_shift, SW_OP));
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
  uint16_t curr_shift = 0;

  vec_push(&asmblr->storage_instr,
           get_i_instruction(asmblr, "$zero", "$k0", upper_addr, LUI_OP));
  vec_push(&asmblr->storage_instr,
           get_i_instruction(asmblr, "$k0", "$k0", lower_addr, ORI_OP));

  // Stage 1: prefix bytes until addr is word-aligned
  while (cur < arg_count && (asmblr->data_address & 3) != 0) {
    if (!have_cur_val) {
      parse_int(args[cur], bytes * 8, &cur_val);
      have_cur_val = true;
      byte_offset = 0;
    }

    // extract next byte (big-endian within element)
    uint8_t b = extract_be_byte(cur_val, bytes, byte_offset);

    // emit: addi $k1, $zero, b ; sb $k1, 0($k0); addi $k0, $k0, 1
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$zero", "$k1", (uint16_t)b, ADDI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k0", "$k1", curr_shift, SB_OP));
    curr_shift++;
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
        parse_int(args[cur], bytes * 8, &cur_val);
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

    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$zero", "$k1", hi, LUI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k1", "$k1", lo, ORI_OP));
    vec_push(&asmblr->storage_instr,
             get_i_instruction(asmblr, "$k0", "$k1", curr_shift, SW_OP));

    curr_shift += 4;

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
                      char* line_label, int r_opcode) {
  if (get_value(&asmblr->labels, imm) != 0xFFFFFFFF) {
    // Immediate is a label: expand to pseudo-instruction sequence

    // Split label into upper and lower 16-bit parts
    char* upper_label = get_upper(imm);
    char* lower_label = get_lower(imm);

    // LUI: load upper 16 bits of label into $at
    add_i_label_instruction(asmblr, line_label, lui_src, "$at", upper_label,
                            lui_op);
    // ORI: add lower 16 bits to $at
    add_i_label_instruction(asmblr, NULL, "$at", "$at", lower_label, ori_op);
    if (r_opcode != 0xFFFF) {
      add_r_instruction(asmblr, NULL, rs, "$at", rt, "0", r_opcode);
    } else {
      // If r_opcode is 0xFFFF, we are doing a load/store memory instruction
      add_r_instruction(asmblr, NULL, rs, "$at", "$at", "0", ADDU_OP);
      add_i_instruction(asmblr, NULL, "$at", rt, "0", opcode);
    }

    free(upper_label);
    free(lower_label);

  } else if (!match_int(imm, 16) && match_int(imm, 32)) {
    // Immediate is larger than 16 bits: use pseudo-instruction
    uint32_t val;
    parse_int(imm, 32, &val);
    uint16_t upper_imm = (val >> 16) & 0xFFFF;  // upper 16 bits
    uint16_t lower_imm = val & 0xFFFF;          // lower 16 bits

    // LUI: load upper 16 bits into $at
    add_i_instruction_int(asmblr, line_label, lui_src, "$at", upper_imm,
                          lui_op);
    // ORI: add lower 16 bits to $at
    add_i_instruction_int(asmblr, NULL, "$at", "$at", lower_imm, ori_op);
    if (r_opcode != 0xFFFF) {
      add_r_instruction(asmblr, NULL, rs, "$at", rt, "0", r_opcode);
    } else {
      // If r_opcode is 0xFFFF, we are doing a load/store memory instruction
      add_i_instruction(asmblr, NULL, "$at", rt, "0", opcode);
    }

  } else {
    // Immediate fits in 16 bits → normal I-type instruction
    add_i_instruction(asmblr, line_label, rs, rt, imm, opcode);
  }
}

static void _branch(Assembler* asmblr, char* rs, char* rt, char* imm,
                    int opcode, char* line_label) {
  // Syntax: branch rs, rt, label/immediate
  // Add ! around label to indicate relative address
  if (get_value(&asmblr->labels, imm) != 0xFFFFFFFF) {
    char* rel_label = malloc(strlen(imm) + 3);
    snprintf(rel_label, strlen(imm) + 3, "!%s!", imm);
    add_i_label_instruction(asmblr, line_label, rs, rt, rel_label, opcode);
    free(rel_label);
  } else {
    add_i_instruction(asmblr, line_label, rs, rt, imm, opcode);
  }
}

static void _branch_immediate(Assembler* asmblr, char* rs, char* rt, char* imm,
                              int opcode, char* line_label) {
  // Syntax: branch rs, label/immediate, label/immediate
  // Load label/immediate into $at first
  // Then branch using $at
  pseudo_immediate(asmblr, "$zero", "$at", rt, ADDI_OP, LUI_OP, ORI_OP, "$zero",
                   NULL, false);
  _branch(asmblr, rs, "$at", imm, opcode, line_label);
}

static void _pseudo_branch(Assembler* asmblr, char* rs, char* rt, char* imm,
                           int branch_opcode, int cmp_op, char* line_label) {
  // Syntax for pseudo-instruction:
  // slt/sltu $at, rs, rt
  // beq/bne $at, $zero, label/immediate
  add_r_instruction(asmblr, NULL, rs, rt, "$at", "0", cmp_op);
  _branch(asmblr, "$at", "$zero", imm, branch_opcode, line_label);
}

static void _pseudo_branch_immediate(Assembler* asmblr, char* rs, char* rt,
                                     char* imm, int branch_opcode, int cmp_op,
                                     char* line_label) {
  // Syntax for pseudo-instruction:
  // Load rt into $at
  // Then pseudo-branch using $at and rs
  // Determine if rs or rt is the immediate
  if (get_value(&asmblr->reg_map, rt) == 0xFFFFFFFF) {
    // rt is immediate
    pseudo_immediate(asmblr, "$zero", "$at", rt, ADDI_OP, LUI_OP, ORI_OP,
                     "$zero", NULL, ADD_OP);
    _pseudo_branch(asmblr, rs, "$at", imm, branch_opcode, cmp_op, line_label);
  } else {
    // rs is immediate
    pseudo_immediate(asmblr, "$zero", "$at", rs, ADDI_OP, LUI_OP, ORI_OP,
                     "$zero", NULL, ADD_OP);
    _pseudo_branch(asmblr, "$at", rt, imm, branch_opcode, cmp_op, line_label);
  }
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

static void GLOBL_dir(Assembler* asmblr, char** args, int arg_count) {
  (void)asmblr;
  (void)arg_count;
  (void)args;
  // Mark label as global
  // In this assembler, all labels are effectively global, so no action needed
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
                   args[1], args[arg_count - 1], ADD_OP);
}

static void ADDIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: addiu rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ADDIU_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1], ADDU_OP);
}

static void AND(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: and rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    AND_OP);
}

static void ANDI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: andi rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], ANDI_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1], AND_OP);
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

static void MULTI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: multi $rs, imm -> li $at, immediate; mult $rs, $at
  pseudo_immediate(asmblr, "$zero", "$at", args[1], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], args[0], "$at", "0", "0",
                    MULT_OP);
}

static void MULTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: multu $rs, $rt
  add_r_instruction(asmblr, args[arg_count - 1], args[0], args[1], "0", "0",
                    MULTU_OP);
}

static void MULTIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: multiu $rs, imm -> li $at, immediate; multu $rs, $at
  pseudo_immediate(asmblr, "$zero", "$at", args[1], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], args[0], "$at", "0", "0",
                    MULTU_OP);
}

static void MUL(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mul rd, rs, rt
  // Expands to mult rs, rt; mflo rd
  add_r_instruction(asmblr, NULL, args[1], args[2], "0", "0", MULT_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", args[0], "0",
                    MFLO_OP);
}

static void MULI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: muli rd, rs, immediate -> li $at, immediate; mult rs, $at; mflo rd
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "0", "0", MULT_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", args[0], "0",
                    MFLO_OP);
}

static void MULU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: mulu rd, rs, rt
  // Expands to multu rs, rt; mflo rd
  add_r_instruction(asmblr, NULL, args[1], args[2], "0", "0", MULTU_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", args[0], "0",
                    MFLO_OP);
}

static void MULIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: muliu rd, rs, imm -> li $at, imm; multu rs, $at; mflo rd
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "0", "0", MULTU_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", args[0], "0",
                    MFLO_OP);
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
                   args[1], args[arg_count - 1], OR_OP);
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

static void SUBI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: subi rt, rs, immediate -> li $at, immediate; sub rd, rs, $at
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], args[1], "$at", args[0], "0",
                    SUB_OP);
}

static void SUBU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: subu rd, rs, rt
  add_r_instruction(asmblr, args[arg_count - 1], args[1], args[2], args[0], "0",
                    SUBU_OP);
}

static void SUBIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: subiu rt, rs, immediate -> li $at, immediate; subu rd, rs, $at
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], args[1], "$at", args[0], "0",
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
                   args[1], args[arg_count - 1], XOR_OP);
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
                   args[1], args[arg_count - 1], SLT_OP);
}

static void SLTIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sltiu rt, rs, immediate
  pseudo_immediate(asmblr, args[1], args[0], args[2], SLTIU_OP, LUI_OP, ORI_OP,
                   args[1], args[arg_count - 1], SLTU_OP);
}

static void SEQ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: seq rd, rs, rt -> sub $at, rs, rt; sltiu rd, $at, 1
  add_r_instruction(asmblr, NULL, args[1], args[2], "$at", "0", SUB_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", SLTIU_OP);
}

static void SEQI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: seqi rd, rs, imm
  // -> li $at, imm; sub $at, rs, $at; sltiu rd, $at, 1
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "$at", "0", SUB_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", SLTIU_OP);
}

static void SNE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sne rd, rs, rt -> sub $at, rs, rt; sltu rd, $zero, $at
  add_r_instruction(asmblr, NULL, args[1], args[2], "$at", "0", SUB_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$at", args[0], "0",
                    SLTU_OP);
}

static void SNEI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: snei rd, rs, imm
  // -> li $at, imm; sub $at, rs, $at; sltu rd, $zero, $at
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "$at", "0", SUB_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$at", args[0], "0",
                    SLTU_OP);
}

static void SGE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sge rd, rs, rt -> slt $at, rs, rt; xori rd, $at, 1
  add_r_instruction(asmblr, NULL, args[1], args[2], "$at", "0", SLT_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SGEI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgei rd, rs, imm
  // -> slti $at, rs, imm; xori rd, $at, 1
  pseudo_immediate(asmblr, args[1], "$at", args[2], SLTI_OP, LUI_OP, ORI_OP,
                   args[1], NULL, SLT_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SGEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgeu rd, rs, rt -> sltu $at, rs, rt; xori rd, $at, 1
  add_r_instruction(asmblr, NULL, args[1], args[2], "$at", "0", SLTU_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SGEIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgeiu rd, rs, imm
  // -> sltiu $at, rs, imm; xori rd, $at, 1
  pseudo_immediate(asmblr, args[1], "$at", args[2], SLTIU_OP, LUI_OP, ORI_OP,
                   args[1], NULL, SLTU_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SGT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgt rd, rs, rt -> slt rd, rt, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[2], args[1], args[0], "0",
                    SLT_OP);
}

static void SGTI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgti rd, rs, imm -> li $at, imm; slt rd, $at, rs
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$at", args[1], args[0], "0",
                    SLT_OP);
}

static void SGTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgtu rd, rs, rt -> sltu rd, rt, rs
  add_r_instruction(asmblr, args[arg_count - 1], args[2], args[1], args[0], "0",
                    SLTU_OP);
}

static void SGTIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sgtiu rd, rs, imm -> li $at, imm; sltu rd, $at, rs
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$at", args[1], args[0], "0",
                    SLTU_OP);
}

static void SLE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sle rd, rs, rt -> slt $at, rt, rs; xori rd, $at, 1
  add_r_instruction(asmblr, NULL, args[2], args[1], "$at", "0", SLT_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SLEI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: slei rd, rs, imm
  // -> li $at, imm; slt $at, $at, rs; xori rd, $at, 1
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, "$at", args[1], "$at", "0", SLT_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SLEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sleu rd, rs, rt -> sltu $at, rt, rs; xori rd, $at, 1
  add_r_instruction(asmblr, NULL, args[2], args[1], "$at", "0", SLTU_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void SLEIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sleiu rd, rs, imm
  // -> li $at, imm; sltu $at, $at, rs; xori rd, $at, 1
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, "$at", args[1], "$at", "0", SLTU_OP);
  add_i_instruction(asmblr, args[arg_count - 1], "$at", args[0], "1", XORI_OP);
}

static void B(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: b label/immediate -> beq $zero, $zero, label/immediate
  _branch(asmblr, "$zero", "$zero", args[0], BEQ_OP, args[arg_count - 1]);
}

static void BEQ(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: beq rs, rt, label/immediate
  _branch(asmblr, args[0], args[1], args[2], BEQ_OP, args[arg_count - 1]);
}

static void BEQ_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: beq rs, label/immediate1, label/immediate2
  _branch_immediate(asmblr, args[0], args[1], args[2], BEQ_OP,
                    args[arg_count - 1]);
}

static void BNE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bne rs, rt, label/immediate
  _branch(asmblr, args[0], args[1], args[2], BNE_OP, args[arg_count - 1]);
}

static void BNE_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bne rs, label/immediate1, label/immediate2
  _branch_immediate(asmblr, args[0], args[1], args[2], BNE_OP,
                    args[arg_count - 1]);
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

static void BLT_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: blt rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[0], args[1], args[2], BNE_OP, SLT_OP,
                           args[arg_count - 1]);
}

static void BGT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgt rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BNE_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BGT_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgt rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[1], args[0], args[2], BNE_OP, SLT_OP,
                           args[arg_count - 1]);
}

static void BLE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: ble rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BEQ_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BLE_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: ble rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[1], args[0], args[2], BEQ_OP, SLT_OP,
                           args[arg_count - 1]);
}

static void BGE(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bge rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BEQ_OP, SLT_OP,
                 args[arg_count - 1]);
}

static void BGE_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bge rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[0], args[1], args[2], BEQ_OP, SLT_OP,
                           args[arg_count - 1]);
}

static void BLTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bltu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BNE_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BLTU_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bltu rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[0], args[1], args[2], BNE_OP, SLTU_OP,
                           args[arg_count - 1]);
}

static void BGTU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgtu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BNE_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BGTU_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgtu rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[1], args[0], args[2], BNE_OP, SLTU_OP,
                           args[arg_count - 1]);
}

static void BLEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bleu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[1], args[0], args[2], BEQ_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BLEU_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bleu rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[1], args[0], args[2], BEQ_OP, SLTU_OP,
                           args[arg_count - 1]);
}

static void BGEU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgeu rs, rt, label/immediate
  _pseudo_branch(asmblr, args[0], args[1], args[2], BEQ_OP, SLTU_OP,
                 args[arg_count - 1]);
}

static void BGEU_imm(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: bgeu rs, label/immediate1, label/immediate2
  _pseudo_branch_immediate(asmblr, args[0], args[1], args[2], BEQ_OP, SLTU_OP,
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
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SB_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sb rt, (base) -> sb rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", SB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SB_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sb rt, offset -> sb rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], SB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SH(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sh rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], SH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SH_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sh rt, (base) -> sh rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", SH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SH_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sh rt, offset -> sh rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], SH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SW(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sw rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], SW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SW_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sw rt, (base) -> sw rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", SW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void SW_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: sw rt, offset -> sw rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], SW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LB(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lb rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LB_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lb rt, (base) -> lb rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", LB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LB_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lb rt, offset -> lb rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], LB_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LBU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lbu rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LBU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LBU_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lbu rt, (base) -> lbu rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", LBU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LBU_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lbu rt, offset -> lbu rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], LBU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LH(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lh rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LH_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lh rt, (base) -> lh rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", LH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LH_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lh rt, offset -> lh rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], LH_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LHU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lhu rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LHU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LHU_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lhu rt, (base) -> lhu rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", LHU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LHU_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lhu rt, offset -> lhu rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], LHU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LW(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lw rt, offset(base)
  pseudo_immediate(asmblr, args[2], args[0], args[1], LW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LW_no_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lw rt, (base) -> lw rt, 0(base)
  pseudo_immediate(asmblr, args[1], args[0], "0", LW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
}

static void LW_only_offset(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: lw rt, offset -> lw rt, offset($zero)
  pseudo_immediate(asmblr, "$zero", args[0], args[1], LW_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], 0xFFFF);
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
                   "$zero", args[arg_count - 1], ADDU_OP);
}

static void LA(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: la rt, label -> addiu rt, $zero, label
  pseudo_immediate(asmblr, "$zero", args[0], args[1], ADDIU_OP, LUI_OP, ORI_OP,
                   "$zero", args[arg_count - 1], ADDU_OP);
}

static void NOP(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: nop -> sll $zero, $zero, 0
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", "$zero", "$zero", "0",
                    SLL_OP);
}

static void NEG(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: neg rd, rs -> sub rd, $zero, rs
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", args[1], args[0], "0",
                    SUB_OP);
}

static void NEGU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: negu rd, rs -> subu rd, $zero, rs
  add_r_instruction(asmblr, args[arg_count - 1], "$zero", args[1], args[0], "0",
                    SUBU_OP);
}

static void NOT(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: not rd, rs -> nor rd, rs, $zero
  add_r_instruction(asmblr, args[arg_count - 1], args[1], "$zero", args[0], "0",
                    NOR_OP);
}

static void REM(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: rem rd, rs, rt -> div rs, rt; mfhi rd
  add_r_instruction(asmblr, NULL, args[1], args[2], "$zero", "0", DIV_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFHI_OP);
}

static void REMI(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: remi rd, rs, imm
  // -> li $at, imm; div rs, $at; mfhi rd
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "$zero", "0", DIV_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFHI_OP);
}

static void REMU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: remu rd, rs, rt -> divu rs, rt; mfhi rd
  add_r_instruction(asmblr, NULL, args[1], args[2], "$zero", "0", DIVU_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFHI_OP);
}

static void REMIU(Assembler* asmblr, char** args, int arg_count) {
  // Syntax: remiu rd, rs, imm
  // -> li $at, imm; divu rs, $at; mfhi rd
  pseudo_immediate(asmblr, "$zero", "$at", args[2], ADDI_OP, LUI_OP, ORI_OP,
                   "$zero", NULL, ADD_OP);
  add_r_instruction(asmblr, NULL, args[1], "$at", "$zero", "0", DIVU_OP);
  add_r_instruction(asmblr, args[arg_count - 1], "$0", "$0", args[0], "0",
                    MFHI_OP);
}

static void init_instructions(Assembler* asmblr) {
  add_dir(asmblr, "data", "", DATA_dir);
  add_dir(asmblr, "text", "", TEXT_dir);
  add_dir(asmblr, "globl", "1", GLOBL_dir);
  add_dir(asmblr, "align", "3", ALIGN_dir);
  add_dir(asmblr, "ascii", "2", ASCII_dir);
  add_dir(asmblr, "asciiz", "2", ASCIIZ_dir);
  add_dir(asmblr, "space", "4", SPACE_dir);
  add_dir(asmblr, "word", ":", WORD_dir);
  add_dir(asmblr, "half", "9", HALF_dir);
  add_dir(asmblr, "byte", "8", BYTE_dir);

  add_cmd(asmblr, "add", "0 0 0", ADD);
  add_cmd(asmblr, "add", "0 0 6", ADDI);
  add_cmd(asmblr, "addu", "0 0 0", ADDU);
  add_cmd(asmblr, "addu", "0 0 6", ADDIU);
  add_cmd(asmblr, "addi", "0 0 6", ADDI);
  add_cmd(asmblr, "addi", "0 0 0", ADD);
  add_cmd(asmblr, "addiu", "0 0 6", ADDIU);
  add_cmd(asmblr, "addiu", "0 0 0", ADDU);
  add_cmd(asmblr, "and", "0 0 0", AND);
  add_cmd(asmblr, "and", "0 0 6", ANDI);
  add_cmd(asmblr, "andi", "0 0 6", ANDI);
  add_cmd(asmblr, "andi", "0 0 0", AND);
  add_cmd(asmblr, "div", "0 0", DIV);
  add_cmd(asmblr, "divu", "0 0", DIVU);
  add_cmd(asmblr, "mult", "0 0", MULT);
  add_cmd(asmblr, "mult", "0 6", MULTI);
  add_cmd(asmblr, "mult", "0 0 0", MUL);
  add_cmd(asmblr, "mult", "0 0 6", MULI);
  add_cmd(asmblr, "multi", "0 6", MULTI);
  add_cmd(asmblr, "multi", "0 0", MULT);
  add_cmd(asmblr, "multi", "0 0 6", MULI);
  add_cmd(asmblr, "multi", "0 0 0", MUL);
  add_cmd(asmblr, "multu", "0 0", MULTU);
  add_cmd(asmblr, "multu", "0 6", MULTIU);
  add_cmd(asmblr, "multu", "0 0 0", MULU);
  add_cmd(asmblr, "multu", "0 0 6", MULIU);
  add_cmd(asmblr, "multiu", "0 6", MULTIU);
  add_cmd(asmblr, "multiu", "0 0", MULTU);
  add_cmd(asmblr, "multiu", "0 0 6", MULIU);
  add_cmd(asmblr, "multiu", "0 0 0", MULU);
  add_cmd(asmblr, "mul", "0 0 0", MUL);
  add_cmd(asmblr, "mul", "0 0 6", MULI);
  add_cmd(asmblr, "mul", "0 0", MULT);
  add_cmd(asmblr, "mul", "0 6", MULTI);
  add_cmd(asmblr, "muli", "0 0 6", MULI);
  add_cmd(asmblr, "muli", "0 0 0", MUL);
  add_cmd(asmblr, "muli", "0 0", MULT);
  add_cmd(asmblr, "muli", "0 6", MULTI);
  add_cmd(asmblr, "mulu", "0 0 0", MULU);
  add_cmd(asmblr, "mulu", "0 0 6", MULIU);
  add_cmd(asmblr, "mulu", "0 0", MULTU);
  add_cmd(asmblr, "mulu", "0 6", MULTIU);
  add_cmd(asmblr, "muliu", "0 0 6", MULIU);
  add_cmd(asmblr, "muliu", "0 0 0", MULU);
  add_cmd(asmblr, "muliu", "0 0", MULTU);
  add_cmd(asmblr, "muliu", "0 6", MULTIU);
  add_cmd(asmblr, "nor", "0 0 0", NOR);
  add_cmd(asmblr, "or", "0 0 0", OR);
  add_cmd(asmblr, "or", "0 0 6", ORI);
  add_cmd(asmblr, "ori", "0 0 6", ORI);
  add_cmd(asmblr, "ori", "0 0 0", OR);
  add_cmd(asmblr, "sll", "0 0 3", SLL);
  add_cmd(asmblr, "sll", "0 0 0", SLLV);
  add_cmd(asmblr, "sllv", "0 0 0", SLLV);
  add_cmd(asmblr, "sllv", "0 0 3", SLL);
  add_cmd(asmblr, "sra", "0 0 3", SRA);
  add_cmd(asmblr, "sra", "0 0 0", SRAV);
  add_cmd(asmblr, "srav", "0 0 0", SRAV);
  add_cmd(asmblr, "srav", "0 0 3", SRA);
  add_cmd(asmblr, "srl", "0 0 3", SRL);
  add_cmd(asmblr, "srl", "0 0 0", SRLV);
  add_cmd(asmblr, "srlv", "0 0 0", SRLV);
  add_cmd(asmblr, "srlv", "0 0 3", SRL);
  add_cmd(asmblr, "sub", "0 0 0", SUB);
  add_cmd(asmblr, "sub", "0 0 6", SUBI);
  add_cmd(asmblr, "subu", "0 0 0", SUBU);
  add_cmd(asmblr, "subu", "0 0 6", SUBIU);
  add_cmd(asmblr, "subi", "0 0 6", SUBI);
  add_cmd(asmblr, "subi", "0 0 0", SUB);
  add_cmd(asmblr, "subiu", "0 0 6", SUBIU);
  add_cmd(asmblr, "subiu", "0 0 0", SUBU);
  add_cmd(asmblr, "xor", "0 0 0", XOR);
  add_cmd(asmblr, "xor", "0 0 6", XORI);
  add_cmd(asmblr, "xori", "0 0 6", XORI);
  add_cmd(asmblr, "xori", "0 0 0", XOR);
  add_cmd(asmblr, "lui", "0 4", LUI);
  add_cmd(asmblr, "slt", "0 0 0", SLT);
  add_cmd(asmblr, "slt", "0 0 6", SLTI);
  add_cmd(asmblr, "sltu", "0 0 0", SLTU);
  add_cmd(asmblr, "sltu", "0 0 6", SLTIU);
  add_cmd(asmblr, "slti", "0 0 6", SLTI);
  add_cmd(asmblr, "slti", "0 0 0", SLT);
  add_cmd(asmblr, "sltiu", "0 0 6", SLTIU);
  add_cmd(asmblr, "sltiu", "0 0 0", SLTU);
  add_cmd(asmblr, "seq", "0 0 0", SEQ);
  add_cmd(asmblr, "seq", "0 0 6", SEQI);
  add_cmd(asmblr, "seqi", "0 0 6", SEQI);
  add_cmd(asmblr, "seqi", "0 0 0", SEQ);
  add_cmd(asmblr, "sne", "0 0 0", SNE);
  add_cmd(asmblr, "sne", "0 0 6", SNEI);
  add_cmd(asmblr, "snei", "0 0 6", SNEI);
  add_cmd(asmblr, "snei", "0 0 0", SNE);
  add_cmd(asmblr, "sge", "0 0 0", SGE);
  add_cmd(asmblr, "sge", "0 0 6", SGEI);
  add_cmd(asmblr, "sgei", "0 0 6", SGEI);
  add_cmd(asmblr, "sgei", "0 0 0", SGE);
  add_cmd(asmblr, "sgeu", "0 0 0", SGEU);
  add_cmd(asmblr, "sgeu", "0 0 6", SGEIU);
  add_cmd(asmblr, "sgeiu", "0 0 6", SGEIU);
  add_cmd(asmblr, "sgeiu", "0 0 0", SGEU);
  add_cmd(asmblr, "sgt", "0 0 0", SGT);
  add_cmd(asmblr, "sgt", "0 0 6", SGTI);
  add_cmd(asmblr, "sgti", "0 0 6", SGTI);
  add_cmd(asmblr, "sgti", "0 0 0", SGT);
  add_cmd(asmblr, "sgtu", "0 0 0", SGTU);
  add_cmd(asmblr, "sgtu", "0 0 6", SGTIU);
  add_cmd(asmblr, "sgtiu", "0 0 6", SGTIU);
  add_cmd(asmblr, "sgtiu", "0 0 0", SGTU);
  add_cmd(asmblr, "sle", "0 0 0", SLE);
  add_cmd(asmblr, "sle", "0 0 6", SLEI);
  add_cmd(asmblr, "slei", "0 0 6", SLEI);
  add_cmd(asmblr, "slei", "0 0 0", SLE);
  add_cmd(asmblr, "sleu", "0 0 0", SLEU);
  add_cmd(asmblr, "sleu", "0 0 6", SLEIU);
  add_cmd(asmblr, "sleiu", "0 0 6", SLEIU);
  add_cmd(asmblr, "sleiu", "0 0 0", SLEU);

  add_cmd(asmblr, "b", "7", B);
  add_cmd(asmblr, "beq", "0 0 7", BEQ);
  add_cmd(asmblr, "beq", "0 7 7", BEQ_imm);
  add_cmd(asmblr, "bne", "0 0 7", BNE);
  add_cmd(asmblr, "bne", "0 7 7", BNE_imm);
  add_cmd(asmblr, "beqz", "0 7", BEQZ);
  add_cmd(asmblr, "bnez", "0 7", BNEZ);
  add_cmd(asmblr, "bltz", "0 7", BLTZ);
  add_cmd(asmblr, "bgez", "0 7", BGEZ);
  add_cmd(asmblr, "bgtz", "0 7", BGTZ);
  add_cmd(asmblr, "blez", "0 7", BLEZ);
  add_cmd(asmblr, "blt", "0 0 7", BLT);
  add_cmd(asmblr, "blt", "0 7 7", BLT_imm);
  add_cmd(asmblr, "bgt", "0 0 7", BGT);
  add_cmd(asmblr, "bgt", "0 7 7", BGT_imm);
  add_cmd(asmblr, "ble", "0 0 7", BLE);
  add_cmd(asmblr, "ble", "0 7 7", BLE_imm);
  add_cmd(asmblr, "bge", "0 0 7", BGE);
  add_cmd(asmblr, "bge", "0 7 7", BGE_imm);
  add_cmd(asmblr, "bltu", "0 0 7", BLTU);
  add_cmd(asmblr, "bltu", "0 7 7", BLTU_imm);
  add_cmd(asmblr, "bgtu", "0 0 7", BGTU);
  add_cmd(asmblr, "bgtu", "0 7 7", BGTU_imm);
  add_cmd(asmblr, "bleu", "0 0 7", BLEU);
  add_cmd(asmblr, "bleu", "0 7 7", BLEU_imm);
  add_cmd(asmblr, "bgeu", "0 0 7", BGEU);
  add_cmd(asmblr, "bgeu", "0 7 7", BGEU_imm);

  add_cmd(asmblr, "j", "1", J);
  add_cmd(asmblr, "jal", "1", JAL);
  add_cmd(asmblr, "jalr", "0 1", JALR);
  add_cmd(asmblr, "jr", "0", JR);
  add_cmd(asmblr, "syscall", "", SYSCALL);

  add_cmd(asmblr, "sb", "0 6(0)", SB);
  add_cmd(asmblr, "sb", "0 (0)", SB_no_offset);
  add_cmd(asmblr, "sb", "0 0", SB_no_offset);
  add_cmd(asmblr, "sb", "0 6", SB_only_offset);
  add_cmd(asmblr, "sb", "0 (6)", SB_only_offset);
  add_cmd(asmblr, "sh", "0 6(0)", SH);
  add_cmd(asmblr, "sh", "0 (0)", SH_no_offset);
  add_cmd(asmblr, "sh", "0 0", SH_no_offset);
  add_cmd(asmblr, "sh", "0 6", SH_only_offset);
  add_cmd(asmblr, "sh", "0 (6)", SH_only_offset);
  add_cmd(asmblr, "sw", "0 6(0)", SW);
  add_cmd(asmblr, "sw", "0 (0)", SW_no_offset);
  add_cmd(asmblr, "sw", "0 0", SW_no_offset);
  add_cmd(asmblr, "sw", "0 6", SW_only_offset);
  add_cmd(asmblr, "sw", "0 (6)", SW_only_offset);
  add_cmd(asmblr, "lb", "0 6(0)", LB);
  add_cmd(asmblr, "lb", "0 (0)", LB_no_offset);
  add_cmd(asmblr, "lb", "0 0", LB_no_offset);
  add_cmd(asmblr, "lb", "0 6", LB_only_offset);
  add_cmd(asmblr, "lb", "0 (6)", LB_only_offset);
  add_cmd(asmblr, "lbu", "0 6(0)", LBU);
  add_cmd(asmblr, "lbu", "0 (0)", LBU_no_offset);
  add_cmd(asmblr, "lbu", "0 0", LBU_no_offset);
  add_cmd(asmblr, "lbu", "0 6", LBU_only_offset);
  add_cmd(asmblr, "lbu", "0 (6)", LBU_only_offset);
  add_cmd(asmblr, "lh", "0 6(0)", LH);
  add_cmd(asmblr, "lh", "0 (0)", LH_no_offset);
  add_cmd(asmblr, "lh", "0 0", LH_no_offset);
  add_cmd(asmblr, "lh", "0 6", LH_only_offset);
  add_cmd(asmblr, "lh", "0 (6)", LH_only_offset);
  add_cmd(asmblr, "lhu", "0 6(0)", LHU);
  add_cmd(asmblr, "lhu", "0 (0)", LHU_no_offset);
  add_cmd(asmblr, "lhu", "0 0", LHU_no_offset);
  add_cmd(asmblr, "lhu", "0 6", LHU_only_offset);
  add_cmd(asmblr, "lhu", "0 (6)", LHU_only_offset);
  add_cmd(asmblr, "lw", "0 6(0)", LW);
  add_cmd(asmblr, "lw", "0 (0)", LW_no_offset);
  add_cmd(asmblr, "lw", "0 0", LW_no_offset);
  add_cmd(asmblr, "lw", "0 6", LW_only_offset);
  add_cmd(asmblr, "lw", "0 (6)", LW_only_offset);
  add_cmd(asmblr, "mfhi", "0", MFHI);
  add_cmd(asmblr, "mflo", "0", MFLO);
  add_cmd(asmblr, "mthi", "0", MTHI);
  add_cmd(asmblr, "mtlo", "0", MTLO);

  add_cmd(asmblr, "move", "0 0", MOVE);
  add_cmd(asmblr, "li", "0 6", LI);
  add_cmd(asmblr, "la", "0 6", LA);
  add_cmd(asmblr, "nop", "", NOP);
  add_cmd(asmblr, "neg", "0 0", NEG);
  add_cmd(asmblr, "negu", "0 0", NEGU);
  add_cmd(asmblr, "not", "0 0", NOT);
  add_cmd(asmblr, "rem", "0 0 0", REM);
  add_cmd(asmblr, "rem", "0 0 6", REMI);
  add_cmd(asmblr, "remi", "0 0 6", REMI);
  add_cmd(asmblr, "remi", "0 0 0", REM);
  add_cmd(asmblr, "remu", "0 0 0", REMU);
  add_cmd(asmblr, "remu", "0 0 6", REMIU);
  add_cmd(asmblr, "remiu", "0 0 6", REMIU);
  add_cmd(asmblr, "remiu", "0 0 0", REMU);
}

#endif

#include "assembler.h"

#include <ctype.h>
#include <execinfo.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "constants.h"
#include "cpu.h"
#include "helper.h"
#include "instructions.h"
#include "opcodes.h"
#include "tables.h"
#include "uthash.h"

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

void strip_comments(Assembler* asmblr) {
  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
    while (*line) {
      if (*line == '"' || *line == '\'') {
        // skip string literals
        char delim = *line;
        line++;
        while (*line && *line != delim) {
          if (*line == '\\') {
            line++;  // skip escape character
            if (*line) line++;
          } else {
            line++;
          }
        }
        if (*line == delim) line++;
      } else if (*line == '#') {
        // found comment, terminate line here
        *line = '\0';
        break;
      } else {
        line++;
      }
    }
  }
}

void replace_commas_with_spaces(Assembler* asmblr) {
  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
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

  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
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
      asmblr->lines.data[dst_index++] = asmblr->lines.data[i];
    } else {
      free(line);
    }
  }

  // Fix array
  vec_splice(&asmblr->lines, dst_index, asmblr->lines.length - dst_index);
}

// Assumes whitespace has been normalized
void merge_labels(Assembler* asmblr) {
  int dst_index = 0;

  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;

    // If line ends with ':', merge with the next line
    int len = strlen(line);
    if (len > 0 && line[len - 1] == ':' && i + 1 < asmblr->lines.length) {
      char* next_line = asmblr->lines.data[i + 1].line;

      // Create merged line
      int new_len = len + 1 + strlen(next_line) + 1;
      char* merged = malloc(new_len);
      snprintf(merged, new_len, "%s %s", line, next_line);

      free(line);
      free(next_line);

      asmblr->lines.data[dst_index].line_num = asmblr->lines.data[i].line_num;
      asmblr->lines.data[dst_index++].line = merged;

      i++;  // skip the next line, already merged
    } else {
      asmblr->lines.data[dst_index].line_num = asmblr->lines.data[i].line_num;
      asmblr->lines.data[dst_index++].line = line;
    }
  }

  // Fix array
  vec_splice(&asmblr->lines, dst_index, asmblr->lines.length - dst_index);
}

// Assume labels have been merged
void normalize_colons(Assembler* asmblr) {
  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
    int len = strlen(line);

    char* buf = malloc(len * 2 + 1);  // worst case: space after every colon
    char* dst = buf;

    for (int j = 0; j < len; j++) {
      *dst++ = line[j];

      // Add space after colon if immediately followed by non-space
      if (line[j] == ':' && j + 1 < len &&
          !isspace((unsigned char)line[j + 1])) {
        *dst++ = ' ';
      }
    }

    *dst = '\0';

    // Replace line
    free(line);
    asmblr->lines.data[i].line = buf;
  }
}

void extract_labels(Assembler* asmblr) {
  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
    char* colon = strchr(line, ':');
    if (colon) {
      // Everything before colon is the label
      int label_len = colon - line;
      char* label = malloc(label_len + 1);
      strncpy(label, line, label_len);
      label[label_len] = '\0';

      // Make sure label is valid, must start with letter or underscore
      // followed by letters, digits, underscores
      if (!(isalpha((unsigned char)label[0]) || label[0] == '_')) {
        error(asmblr, asmblr->lines.data[i].line_num);
      } else {
        for (int j = 1; j < label_len; j++) {
          if (!(isalnum((unsigned char)label[j]) || label[j] == '_')) {
            error(asmblr, asmblr->lines.data[i].line_num);
          }
        }
      }

      asmblr->lines.data[i].label = label;
      // Save label to table (address will be filled in later)
      // 0xFFFFFFFE indicates unknown address for now
      // 0xFFFFFFFF reserved for undefined labels
      add_value(&asmblr->labels, label, 0xFFFFFFFE);

      // Skip the colon and any space immediately after
      char* rest = colon + 1;
      while (*rest && isspace((unsigned char)*rest)) rest++;

      // Shift rest of line left if any
      if (*rest) {
        char* new_line = strdup(rest);
        free(asmblr->lines.data[i].line);
        asmblr->lines.data[i].line = new_line;
      }
    }
  }
}

// Assume labels have been extracted
void lowercase_instructions(Assembler* asmblr) {
  for (int i = 0; i < asmblr->lines.length; i++) {
    char* line = asmblr->lines.data[i].line;
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

          int len = token_end - token_start;
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

        int len = token_end - token_start;
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
            valid = match_int(value, 16);
            break;
          case INT32:
            valid = match_int(value, 32);
            break;
          case IMMEDIATE:
            valid = match_label(asmblr, value) || match_int(value, 32);
            break;
          case SHORT_IMMEDIATE:
            valid = match_label(asmblr, value) || match_int(value, 16);
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
  for (int i = 0; i < asmblr->lines.length; i++) {
    const char* orig_line = asmblr->lines.data[i].line;
    char* line = strdup(orig_line);
    if (!line) {
      error(asmblr, asmblr->lines.data[i].line_num);
      return;
    }

    char* mnemonic = strtok(line, " ");
    char* rest = strtok(NULL, "");
    if (!mnemonic) {
      free(line);
      continue;
    }

    if (mnemonic[0] == '.') {
      Instruction* d = get_dir(asmblr, mnemonic + 1);
      if (!d) {
        error(asmblr, asmblr->lines.data[i].line_num);
        free(line);
        return;
      }

      // Always handle .data and .text to switch context
      if (strcmp(d->name, "data") == 0 || strcmp(d->name, "text") == 0) {
        d->tmpls.data[0].handler(asmblr, NULL, 0);
      }
      // Only compile if current section matches the pass
      else if (asmblr->is_data == compile_data) {
        int arg_count = 0;
        char** args = NULL;
        for (int t = 0; t < d->tmpl_count; t++) {
          args = parse_template(asmblr, d->tmpls.data[t].tmpl,
                                asmblr->lines.data[i].label, rest, &arg_count);
          if (args) {
            d->tmpls.data[t].handler(asmblr, args, arg_count);
            for (int j = 0; j < arg_count; j++) free(args[j]);
            free(args);
            break;
          }
        }
        if (!args) {
          error(asmblr, asmblr->lines.data[i].line_num);
          free(line);
          return;
        }

        if (strcmp(d->name, "align") != 0) asmblr->align = 0;
      }
    } else {
      // Only compile commands in text pass
      if (!compile_data && !asmblr->is_data) {
        Instruction* c = get_cmd(asmblr, mnemonic);
        if (!c) {
          error(asmblr, asmblr->lines.data[i].line_num);
          free(line);
          return;
        }

        int arg_count = 0;
        char** args = NULL;
        for (int t = 0; t < c->tmpl_count; t++) {
          args = parse_template(asmblr, c->tmpls.data[t].tmpl,
                                asmblr->lines.data[i].label, rest, &arg_count);
          if (args) {
            c->tmpls.data[t].handler(asmblr, args, arg_count);
            for (int j = 0; j < arg_count; j++) free(args[j]);
            free(args);
            break;
          }
        }
        if (!args) {
          error(asmblr, asmblr->lines.data[i].line_num);
          free(line);
          return;
        }
      }
    }

    free(line);
  }
}

vec_uint32_t* compile(Assembler* asmblr, int* out_size) {
  strip_comments(asmblr);
  replace_commas_with_spaces(asmblr);
  normalize_whitespace(asmblr);
  merge_labels(asmblr);
  normalize_colons(asmblr);
  extract_labels(asmblr);
  lowercase_instructions(asmblr);

  // First pass: compile .data section
  compile_section(asmblr, true);
  if (asmblr->error) return NULL;
  // Second pass: compile .text section
  compile_section(asmblr, false);
  if (asmblr->error) return NULL;

  // Add j main to end of storage instructions
  // Enough space for opcode, @main@, and null terminator
  char* instr_str = malloc(13);
  char* opcode_str = int_to_binary((uint32_t)J_OP, 6);
  snprintf(instr_str, 13, "%s@%s@", opcode_str, "main");
  free(opcode_str);
  vec_push(&asmblr->storage_instr, instr_str);

  // Combine storage and intermediate instructions into single array
  int total_instructions = asmblr->storage_instr.length + asmblr->intmd.length;
  LineData* all_instructions = malloc(sizeof(LineData) * total_instructions);

  for (int i = 0; i < asmblr->storage_instr.length; i++) {
    all_instructions[i].line = strdup(asmblr->storage_instr.data[i]);
    all_instructions[i].label = NULL;
  }
  for (int j = 0; j < asmblr->intmd.length; j++) {
    LineData* src = &asmblr->intmd.data[j];
    LineData* dst = &all_instructions[asmblr->storage_instr.length + j];
    dst->line = src->line ? strdup(src->line) : NULL;
    dst->label = src->label ? strdup(src->label) : NULL;
  }

  // Add labels to hash table (data labels have already been handled)
  for (int i = asmblr->storage_instr.length; i < total_instructions; i++) {
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
  for (int i = 0; i < total_instructions; i++) {
    char* line = all_instructions[i].line;
    char* new_line = malloc(50);  // 32 bits + null terminator
    char* dst = new_line;

    for (char* src = line; *src;) {
      if (*src == '^' || *src == '.' || *src == '@' || *src == '!') {
        char marker = *src++;
        char* start = src;
        while (*src && *src != marker) src++;
        int lablen = src - start;
        char label[lablen + 1];
        memcpy(label, start, lablen);
        label[lablen] = '\0';

        // Lookup label address
        StrToUint32* label_entry;
        HASH_FIND_STR(asmblr->labels, label, label_entry);
        if (!label_entry) {
          // Undefined label
          if (i >= asmblr->storage_instr.length) {
            error(
                asmblr,
                asmblr->lines.data[i - asmblr->storage_instr.length].line_num);
          } else {
            // Only happens if main function label is missing
            error(asmblr, -1);
          }

          free(new_line);
          // Free all_instructions
          for (int k = 0; k < total_instructions; k++) {
            free(all_instructions[k].line);
            if (all_instructions[k].label) free(all_instructions[k].label);
          }
          free(all_instructions);
          return NULL;
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

        int rep_len = strlen(replacement);
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

  // Get max label length
  int max_label_len = 0;
  for (int i = 0; i < asmblr->lines.length; i++) {
    if (asmblr->lines.data[i].label) {
      int len = strlen(asmblr->lines.data[i].label);
      if (len > max_label_len) {
        max_label_len = len;
      }
    }
  }

  // Print asmblr lines for debugging
  printf("\nAssembled Lines:\n");
  for (int i = 0; i < asmblr->lines.length; i++) {
    if (asmblr->lines.data[i].label) {
      printf("%-*s: %s\n", (int)max_label_len, asmblr->lines.data[i].label,
             asmblr->lines.data[i].line);
    } else {
      printf("%-*s  %s\n", (int)max_label_len, "", asmblr->lines.data[i].line);
    }
  }

  vec_uint32_t* machine_code = malloc(sizeof(vec_uint32_t));
  vec_init(machine_code);

  // Convert all_instructions to machine code
  for (int i = 0; i < total_instructions; i++) {
    vec_push(machine_code, binary_to_int(all_instructions[i].line));
    free(all_instructions[i].line);
    if (all_instructions[i].label) free(all_instructions[i].label);
  }
  *out_size = total_instructions;
  free(all_instructions);

  return machine_code;
}

vec_uint32_t* assemble(char* filename, int* out_size) {
  // Load source file
  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error: Could not open file %s\n", filename);
    return NULL;
  }

  // Initialize assembler
  Assembler* asmblr = (Assembler*)malloc(sizeof(Assembler));

  vec_init(&asmblr->og_lines);
  vec_init(&asmblr->lines);
  vec_init(&asmblr->intmd);
  vec_init(&asmblr->storage_instr);

  asmblr->is_data = false;
  asmblr->error = false;
  asmblr->labels = NULL;
  asmblr->reg_map = NULL;
  asmblr->dir_map = NULL;
  asmblr->cmd_map = NULL;
  asmblr->align = 0;
  asmblr->data_address = DATA_START_ADDRESS;

  init_registers(&asmblr->reg_map);
  init_instructions(asmblr);

  // Read lines from file
  char* line = NULL;
  size_t len = 0;
  ssize_t read;
  uint32_t line_num = 0;

  while ((read = getline(&line, &len, file)) != -1) {
    // Strip newline character
    if (read > 0 && line[read - 1] == '\n') {
      line[read - 1] = '\0';
    }
    LineData ld1;
    ld1.line = strdup(line);
    ld1.label = NULL;
    LineData ld2;
    ld2.line = strdup(line);
    ld2.label = NULL;
    ld2.line_num = line_num;
    vec_push(&asmblr->og_lines, ld1);
    vec_push(&asmblr->lines, ld2);
    line_num++;
  }
  free(line);
  fclose(file);

  // Construct machine code
  vec_uint32_t* machine_code = compile(asmblr, out_size);

  // Free assembler except machine code
  for (int i = 0; i < asmblr->lines.length; i++) {
    free(asmblr->lines.data[i].line);
    if (asmblr->lines.data[i].label) free(asmblr->lines.data[i].label);
  }
  vec_deinit(&asmblr->lines);

  for (int i = 0; i < asmblr->og_lines.length; i++) {
    free(asmblr->og_lines.data[i].line);
  }
  vec_deinit(&asmblr->og_lines);

  for (int i = 0; i < asmblr->intmd.length; i++) {
    free(asmblr->intmd.data[i].line);
    if (asmblr->intmd.data[i].label) free(asmblr->intmd.data[i].label);
  }
  vec_deinit(&asmblr->intmd);

  for (int i = 0; i < asmblr->storage_instr.length; i++) {
    free(asmblr->storage_instr.data[i]);
  }
  vec_deinit(&asmblr->storage_instr);

  for (int i = 0; i < asmblr->storage_instr.length; i++) {
    free(asmblr->storage_instr.data[i]);
  }
  vec_deinit(&asmblr->storage_instr);

  // Free directives/commands
  Instruction *dir_entry, *dir_tmp;
  HASH_ITER(hh, asmblr->dir_map, dir_entry, dir_tmp) {
    HASH_DEL(asmblr->dir_map, dir_entry);
    free(dir_entry->name);
    for (int i = 0; i < dir_entry->tmpl_count; i++) {
      free(dir_entry->tmpls.data[i].tmpl);
    }
    vec_deinit(&dir_entry->tmpls);
    free(dir_entry);
  }
  Instruction *cmd_entry, *cmd_tmp;
  HASH_ITER(hh, asmblr->cmd_map, cmd_entry, cmd_tmp) {
    HASH_DEL(asmblr->cmd_map, cmd_entry);
    free(cmd_entry->name);
    for (int i = 0; i < cmd_entry->tmpl_count; i++) {
      free(cmd_entry->tmpls.data[i].tmpl);
    }
    vec_deinit(&cmd_entry->tmpls);
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
    if (machine_code != NULL) vec_deinit_full(machine_code);
    free(asmblr);
    return NULL;
  }
  free(asmblr);

  return machine_code;
}
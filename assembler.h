#ifndef _ASSEMBLER_H_
#define _ASSEMBLER_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "array.h"
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
  uint32_t line_num;  // Original line number in source file
} LineData;

struct Assembler;

typedef struct TemplateExec {
  char* tmpl;
  void (*handler)(struct Assembler* asmblr, char** args, int arg_count);
} TemplateExec;

typedef struct Instruction {
  char* name;
  vec_t(TemplateExec) tmpls;  // Array of templates
  uint16_t tmpl_count;        // Number of templates
  UT_hash_handle hh;
} Instruction;

typedef struct Assembler {
  vec_t(LineData) lines;       // Array of lines from the source file
  vec_t(LineData) og_lines;    // Original lines (for error reporting)
  bool is_data;                // 1 = .data section, 0 = .text section
  bool error;                  // Whether an error has occurred
  StrToUint32* labels;         // Hash table of labels to addresses
  StrToUint32* reg_map;        // Hash table of register names to numbers
  Instruction* dir_map;        // Hash table of directives
  Instruction* cmd_map;        // Hash table of commands
  vec_t(LineData) intmd;       // Plain-text machine code with labels
  vec_t(char*) storage_instr;  // Plain-text machine code (kernel storage)
  uint8_t align;               // Current alignment for .data section
  uint32_t data_address;       // Current data segment address
} Assembler;

// Assemble assembly file into machine code
vec_uint32_t* assemble(char* filename, int* out_size);
bool match_int(char* line, int base);

#endif
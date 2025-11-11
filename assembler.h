#ifndef _ASSEMBLER_H_
#define _ASSEMBLER_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// Assemble assembly file into machine code
uint32_t* assemble(char* filename, size_t* out_size);
bool match_int(char* line, int base);

#endif
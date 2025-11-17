#ifndef _CPU_H_
#define _CPU_H_

#include <stdbool.h>
#include <stdint.h>

#include "array.h"
#include "constants.h"
#include "tables.h"
#include "uthash.h"

typedef enum Error {
  NO_ERROR,
  MEMORY_ALIGNMENT,
  PROGRAM_OUT_OF_BOUNDS,
  ARITHMETIC_OVERFLOW,
  DIVIDE_BY_ZERO,
  UNKNOWN_INSTRUCTION,
  UNKNOWN_SYSCALL,
  MEMORY_OUT_OF_BOUNDS
} Error;

typedef struct CPU {
  uint32_t regs[34];  // 32 general purpose + HI + LO
  AddrData* memory;   // Stores all program, data, and stack memory
  uint16_t program_size;
  uint32_t pc;
  Error error;
  uint16_t error_address;
} CPU;

typedef struct RParams {
  uint8_t rs;
  uint8_t rt;
  uint8_t rd;
  uint8_t shamt;
} RParams;

// R-format instruction structure
typedef struct RInstruction {
  bool inc_pc;
  uint8_t funct;
  void (*execute)(CPU* cpu, RParams* params);
} RInstruction;

typedef struct IParams {
  uint8_t rs;
  uint8_t rt;
  uint16_t immediate;
} IParams;

// I-format instruction structure
typedef struct IInstruction {
  bool inc_pc;
  uint8_t opcode;
  void (*execute)(CPU* cpu, IParams* params);
} IInstruction;

typedef struct JParams {
  uint32_t address;
} JParams;

// J-format instruction structure
typedef struct JInstruction {
  uint8_t opcode;
  void (*execute)(CPU* cpu, JParams* params);
} JInstruction;

// Initialize a new CPU instance
CPU* CPU_init();
// Destroy a CPU instance
void CPU_destroy(CPU* cpu);
// Run the program loaded in program memory
void run_program(CPU* cpu);
// Load a program from a file into CPU program memory
CPU* load_program(char* filename);

#endif
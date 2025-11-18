#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "assembler.h"
#include "cpu.h"
#include "helper.h"

#define LOADING "out.txt"

int main() {
  // Load CPU and program
  CPU* cpu = load_program(LOADING);
  if (cpu == NULL) {
    fprintf(stderr, "Failed to load program from %s\n", LOADING);
    return 1;
  }

  // Run program
  int code = run_program(cpu);

  // Print memory
  // for (uint32_t addr = DATA_START_ADDRESS; addr < DATA_START_ADDRESS + 100;
  //      addr += 4) {
  //   uint32_t value;
  //   if (get_data(&cpu->memory, addr, &value)) {
  //     printf("0x%08X: 0x%08X\n", addr, value);
  //   }
  // }
  // print registers
  // for (int i = 0; i < 34; i++) {
  //   printf("R%d: 0x%08X\n", i, cpu->regs[i]);
  // }

  CPU_destroy(cpu);

  return code;
}
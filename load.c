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
  run_program(cpu);

  CPU_destroy(cpu);

  return 0;
}
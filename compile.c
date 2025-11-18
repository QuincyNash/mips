#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "assembler.h"
#include "helper.h"

#define LOADING "hangman.asm"
#define OUTPUT "out.txt"

int main() {
  // Assemble file
  int out_size = 0;
  vec_uint32_t* machine_code = assemble(LOADING, &out_size);

  if (machine_code == NULL) {
    return 1;
  } else {
    // Save machine code to file
    FILE* outfile = fopen(OUTPUT, "w");
    if (outfile == NULL) {
      fprintf(stderr, "Error: Could not open output file %s\n", OUTPUT);
      vec_deinit(machine_code);
      return 1;
    }
    for (int i = 0; i < out_size; i++) {
      char* bin_str = int_to_binary(machine_code->data[i], 32);
      fprintf(outfile, "%s", bin_str);
      if (i < out_size - 1) {
        fprintf(outfile, "\n");
      }
      free(bin_str);
    }
    fclose(outfile);
    printf("Assembly successful. Machine code written to %s\n", OUTPUT);
    vec_deinit_full(machine_code);
  }

  return 0;
}
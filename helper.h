#ifndef _HELPER_H_
#define _HELPER_H_

#include <stdbool.h>
#include <stdint.h>

// Unescape a C string literal (strip quotes)
char* unescape_c_string(const char* str);
// Parse integer from string with given bit size, return true on success
bool parse_int(const char* str, int bits, uint32_t* out_val);
// Convert integer to binary string representation
char* int_to_binary(uint32_t value, int bits);
// Convert 32-bit binary string to integer
uint32_t binary_to_int(const char* str);
// Sets a random seed based on time and process ID
void set_random_seed(void);
// Returns a random int in the range [0, UINT32_MAX]
uint32_t random_int(void);
// Returns a random int in the range [lo, hi]
uint32_t random_range(uint32_t lo, uint32_t hi);

#endif
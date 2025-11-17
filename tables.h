#ifndef _TABLES_H_
#define _TABLES_H_

#include <stdbool.h>
#include <stdint.h>

#include "uthash.h"

typedef struct StrToUint32 {
  char* key;
  uint32_t value;
  UT_hash_handle hh;
} StrToUint32;

typedef struct AddrData {
  uint32_t address;
  uint32_t data;
  UT_hash_handle hh;
} AddrData;

typedef struct ErrData {
  uint32_t intmd_line;
  uint32_t orig_line;
  UT_hash_handle hh;
} ErrData;

// Register table functions
void add_value(StrToUint32** map, const char* name, uint32_t number);
uint32_t get_value(StrToUint32** map, const char* name);
// Error table functions
void add_error(ErrData** map, uint32_t intmd_line, uint32_t orig_line);
bool get_error(ErrData** map, uint32_t intmd_line, uint32_t* out_orig_line);
// Memory table functions
void add_data(AddrData** map, uint32_t address, uint32_t data);
bool get_data(AddrData** map, uint32_t address, uint32_t* out_data);
// Initialize register table
void init_registers(StrToUint32** map);

#endif
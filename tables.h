#ifndef _TABLES_H_
#define _TABLES_H_

#include <stdint.h>

#include "uthash.h"

typedef struct StrToUint32 {
  char* key;
  uint32_t value;
  UT_hash_handle hh;
} StrToUint32;

// Register table functions
void add_value(StrToUint32** map, const char* name, uint32_t number);
uint32_t get_value(StrToUint32** map, const char* name);
// Initialize register table
void init_registers(StrToUint32** map);

#endif
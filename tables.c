#include "tables.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "uthash.h"

void add_value(StrToUint32** map, const char* name, uint32_t number) {
  StrToUint32* r = malloc(sizeof(StrToUint32));
  r->key = strdup(name);
  r->value = number;
  HASH_ADD_KEYPTR(hh, *map, r->key, strlen(r->key), r);
}

uint32_t get_value(StrToUint32** map, const char* name) {
  StrToUint32* r;
  HASH_FIND_STR(*map, name, r);
  return r ? r->value : 0xFFFFFFFF;
}

void add_data(AddrData** map, uint32_t address, uint32_t data) {
  AddrData* r;
  HASH_FIND(hh, *map, &address, sizeof(uint32_t), r);

  if (r) {
    r->data = data;  // update existing
  } else {
    r = malloc(sizeof(AddrData));
    r->address = address;
    r->data = data;
    HASH_ADD(hh, *map, address, sizeof(uint32_t), r);
  }
}

bool get_data(AddrData** map, uint32_t address, uint32_t* out_data) {
  AddrData* r;
  HASH_FIND(hh, *map, &address, sizeof(uint32_t), r);
  if (r != NULL) {
    if (out_data != NULL) *out_data = r->data;
    return true;
  }
  return false;
}

void init_registers(StrToUint32** map) {
  add_value(map, "$zero", 0);
  add_value(map, "$at", 1);
  add_value(map, "$v0", 2);
  add_value(map, "$v1", 3);
  add_value(map, "$a0", 4);
  add_value(map, "$a1", 5);
  add_value(map, "$a2", 6);
  add_value(map, "$a3", 7);
  add_value(map, "$t0", 8);
  add_value(map, "$t1", 9);
  add_value(map, "$t2", 10);
  add_value(map, "$t3", 11);
  add_value(map, "$t4", 12);
  add_value(map, "$t5", 13);
  add_value(map, "$t6", 14);
  add_value(map, "$t7", 15);
  add_value(map, "$s0", 16);
  add_value(map, "$s1", 17);
  add_value(map, "$s2", 18);
  add_value(map, "$s3", 19);
  add_value(map, "$s4", 20);
  add_value(map, "$s5", 21);
  add_value(map, "$s6", 22);
  add_value(map, "$s7", 23);
  add_value(map, "$t8", 24);
  add_value(map, "$t9", 25);
  add_value(map, "$k0", 26);
  add_value(map, "$k1", 27);
  add_value(map, "$gp", 28);
  add_value(map, "$sp", 29);
  add_value(map, "$fp", 30);
  add_value(map, "$ra", 31);
  add_value(map, "$0", 0);
  add_value(map, "$1", 1);
  add_value(map, "$2", 2);
  add_value(map, "$3", 3);
  add_value(map, "$4", 4);
  add_value(map, "$5", 5);
  add_value(map, "$6", 6);
  add_value(map, "$7", 7);
  add_value(map, "$8", 8);
  add_value(map, "$9", 9);
  add_value(map, "$10", 10);
  add_value(map, "$11", 11);
  add_value(map, "$12", 12);
  add_value(map, "$13", 13);
  add_value(map, "$14", 14);
  add_value(map, "$15", 15);
  add_value(map, "$16", 16);
  add_value(map, "$17", 17);
  add_value(map, "$18", 18);
  add_value(map, "$19", 19);
  add_value(map, "$20", 20);
  add_value(map, "$21", 21);
  add_value(map, "$22", 22);
  add_value(map, "$23", 23);
  add_value(map, "$24", 24);
  add_value(map, "$25", 25);
  add_value(map, "$26", 26);
  add_value(map, "$27", 27);
  add_value(map, "$28", 28);
  add_value(map, "$29", 29);
  add_value(map, "$30", 30);
  add_value(map, "$31", 31);
}

#include "helper.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

char* int_to_binary(uint32_t value, int bits) {
  char* str = malloc(bits + 1);
  if (!str) return NULL;

  for (int i = bits - 1; i >= 0; i--) {
    str[i] = (value & 1) ? '1' : '0';
    value >>= 1;
  }
  str[bits] = '\0';
  return str;
}

uint32_t binary_to_int(const char* str) {
  uint32_t value = 0;
  for (int i = 0; str[i]; i++) {
    value <<= 1;
    if (str[i] == '1') {
      value |= 1;
    }
  }
  return value;
}

bool parse_int(const char* str, int bits, uint32_t* out_val) {
  if (bits <= 0 || bits > 32) return false;

  const char* p = str;
  while (*p && isspace((unsigned char)*p)) p++;

  int negative = 0;
  if (*p == '+' || *p == '-') {
    if (*p == '-') negative = 1;
    p++;
  }

  int base = 10;
  if (*p == '0') {
    if (*(p + 1) == 'x' || *(p + 1) == 'X') {
      base = 16;
      p += 2;
    } else if (*(p + 1) == 'b' || *(p + 1) == 'B') {
      base = 2;
      p += 2;
    }
  }

  if (!*p) return false;  // empty after prefix

  uint64_t val = 0;
  while (*p) {
    char c = *p;
    int digit;
    if (isdigit((unsigned char)c))
      digit = c - '0';
    else if (base == 16 && c >= 'a' && c <= 'f')
      digit = 10 + (c - 'a');
    else if (base == 16 && c >= 'A' && c <= 'F')
      digit = 10 + (c - 'A');
    else
      break;

    if (digit >= base) return false;

    val = val * base + digit;
    // Early overflow check for unsigned representation
    if (val > (1ULL << bits)) return false;
    p++;
  }

  if (*p != '\0') return false;  // extra junk at the end

  if (negative) {
    if (val > (1ULL << bits)) return false;
    val = (~val + 1) & ((1ULL << bits) - 1);  // two's complement
  } else {
    if (val >= (1ULL << bits)) return false;
  }

  if (out_val != NULL) *out_val = (uint32_t)val;
  return true;
}

char* unescape_c_string(const char* str) {
  if (!str) return NULL;

  size_t len = strlen(str);
  if (len < 2) return NULL;

  char quote = str[0];
  if ((quote != '"' && quote != '\'') || str[len - 1] != quote) {
    return NULL;  // not a valid quoted string
  }

  const char* src = str + 1;        // skip leading quote
  const char* end = str + len - 1;  // skip trailing quote

  char* out = malloc(end - src + 1);  // worst case: no escapes
  if (!out) return NULL;

  char* dst = out;
  while (src < end) {
    if (*src == '\\') {
      src++;
      if (src >= end) break;

      char c;
      if (*src >= '0' && *src <= '7') {
        // Octal escape \nnn
        int val = 0, count = 0;
        while (count < 3 && src < end && *src >= '0' && *src <= '7') {
          val = val * 8 + (*src - '0');
          src++;
          count++;
        }
        c = (char)val;
        src--;  // adjust for increment later
      } else {
        switch (*src) {
          case 'n':
            c = '\n';
            break;
          case 't':
            c = '\t';
            break;
          case 'r':
            c = '\r';
            break;
          case '"':
            c = '"';
            break;
          case '\'':
            c = '\'';
            break;
          case '\\':
            c = '\\';
            break;
          case '0':
            c = '\0';
            break;
          case 'x': {
            // Hex escape \xNN
            src++;
            int val = 0, digits = 0;
            while (digits < 2 && src < end && isxdigit((unsigned char)*src)) {
              val *= 16;
              if (isdigit((unsigned char)*src))
                val += *src - '0';
              else
                val += tolower((unsigned char)*src) - 'a' + 10;
              src++;
              digits++;
            }
            c = (char)val;
            src--;  // adjust
            break;
          }
          default:
            c = *src;  // unknown escape, keep literally
            break;
        }
      }
      *dst++ = c;
    } else {
      *dst++ = *src;
    }
    src++;
  }

  *dst = '\0';
  return out;
}

typedef struct {
  uint64_t state;
  uint64_t inc;
} pcg32_random_t;

// Static so it's only accessible within this file
static pcg32_random_t rng;

static uint32_t pcg32_random_r(pcg32_random_t* rng) {
  uint64_t oldstate = rng->state;
  rng->state = oldstate * 6364136223846793005ULL + (rng->inc | 1);
  uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
  uint32_t rot = oldstate >> 59u;
  return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

static void pcg32_srandom_r(pcg32_random_t* rng, uint64_t initstate,
                            uint64_t initseq) {
  rng->state = 0U;
  rng->inc = (initseq << 1u) | 1u;
  pcg32_random_r(rng);
  rng->state += initstate;
  pcg32_random_r(rng);
}

void set_random_seed(void) {
  uint64_t seed1 = (uint64_t)time(NULL);
  uint64_t seed2 = ((uint64_t)getpid() << 32) ^ (uint64_t)clock();
  pcg32_srandom_r(&rng, seed1, seed2);
}

uint32_t random_int(void) { return pcg32_random_r(&rng); }

uint32_t random_range(uint32_t lo, uint32_t hi) {
  if (hi < lo) {
    uint32_t t = lo;
    lo = hi;
    hi = t;
  } else if (hi == lo) {
    return lo;
  }
  uint32_t span = (uint32_t)(hi - lo + 1U);
  return lo + (random_int() % span);
}

#include "cpu.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opcodes.h"

#define HI 32
#define LO 33

void CPU_destroy(CPU* cpu) { free(cpu); }

uint32_t get(CPU* cpu, uint8_t reg) { return cpu->regs[reg]; }

int32_t get_signed(CPU* cpu, uint8_t reg) { return (int32_t)(cpu->regs[reg]); }

void set(CPU* cpu, uint8_t reg, uint32_t value) {
  if (reg != 0 && cpu->error == NO_ERROR) {
    cpu->regs[reg] = value;
  }
}

void throw(CPU* cpu, Error error) {
  cpu->error = error;
  cpu->error_address = cpu->pc;
}

uint32_t get_memory(CPU* cpu, uint32_t address) {
  if (address < TEXT_START_ADDRESS || address > STACK_START_ADDRESS) {
    throw(cpu, MEMORY_OUT_OF_BOUNDS);
    return 0;
  }
  if (address >= DATA_START_ADDRESS + MEMORY_SIZE) {
    if (STACK_START_ADDRESS - address >= STACK_SIZE) {
      throw(cpu, MEMORY_OUT_OF_BOUNDS);
      return 0;
    }
    uint32_t offset = STACK_START_ADDRESS - address;
    return cpu->stack[offset / 4];
  } else if (address >= DATA_START_ADDRESS) {
    uint32_t offset = address - DATA_START_ADDRESS;
    return cpu->memory[offset / 4];
  } else {
    if (address - TEXT_START_ADDRESS >= PROGRAM_SIZE * 4) {
      throw(cpu, MEMORY_OUT_OF_BOUNDS);
      return 0;
    }
    uint32_t offset = address - TEXT_START_ADDRESS;
    return cpu->program[offset / 4];
  }
}

void set_memory(CPU* cpu, uint32_t address, uint32_t value) {
  if (cpu->error != NO_ERROR) return;

  if (address < DATA_START_ADDRESS || address > STACK_START_ADDRESS) {
    throw(cpu, MEMORY_OUT_OF_BOUNDS);
    return;
  }
  if (address >= DATA_START_ADDRESS + MEMORY_SIZE) {
    if (STACK_START_ADDRESS - address >= STACK_SIZE) {
      throw(cpu, MEMORY_OUT_OF_BOUNDS);
      return;
    }
    uint32_t offset = STACK_START_ADDRESS - address;
    cpu->stack[offset / 4] = value;
  } else if (address >= DATA_START_ADDRESS) {
    uint32_t offset = address - DATA_START_ADDRESS;
    cpu->memory[offset / 4] = value;
  } else {
    throw(cpu, MEMORY_OUT_OF_BOUNDS);
    return;
  }
}

static void ADD(CPU* cpu, struct RParams* p) {
  int32_t rs = get(cpu, p->rs);
  int32_t rt = get(cpu, p->rt);
  int32_t s = rs + rt;

  if (((rs > 0 && rt > 0 && s < 0) || (rs < 0 && rt < 0 && s > 0))) {
    throw(cpu, ARITHMETIC_OVERFLOW);
  } else {
    set(cpu, p->rd, (uint32_t)s);
  }
}

static void ADDU(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) + get(cpu, p->rt));
}

static void ADDI(CPU* cpu, struct IParams* p) {
  int32_t rs = get_signed(cpu, p->rs);
  int32_t imm = (int16_t)(p->immediate);
  int32_t s = rs + imm;

  if (((rs > 0 && imm > 0 && s < 0) || (rs < 0 && imm < 0 && s > 0))) {
    throw(cpu, ARITHMETIC_OVERFLOW);
  } else {
    set(cpu, p->rt, (uint32_t)s);
  }
}

static void ADDIU(CPU* cpu, struct IParams* p) {
  int32_t rs = get_signed(cpu, p->rs);
  int32_t imm = (int16_t)(p->immediate);
  set(cpu, p->rt, (uint32_t)(rs + imm));
}

static void AND(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) & get(cpu, p->rt));
}

static void ANDI(CPU* cpu, struct IParams* p) {
  set(cpu, p->rt, get(cpu, p->rs) & (uint32_t)(p->immediate));
}

static void DIV(CPU* cpu, RParams* p) {
  int32_t rs = get_signed(cpu, p->rs);
  int32_t rt = get_signed(cpu, p->rt);
  if (rt == 0) {
    throw(cpu, DIVIDE_BY_ZERO);
    return;
  }
  set(cpu, HI, (uint32_t)(rs % rt));
  set(cpu, LO, (uint32_t)(rs / rt));
}

static void DIVU(CPU* cpu, RParams* p) {
  uint32_t rs = get(cpu, p->rs);
  uint32_t rt = get(cpu, p->rt);
  if (rt == 0) {
    throw(cpu, DIVIDE_BY_ZERO);
    return;
  }
  set(cpu, HI, rs % rt);
  set(cpu, LO, rs / rt);
}

static void MULT(CPU* cpu, RParams* p) {
  int64_t rs = (int64_t)get_signed(cpu, p->rs);
  int64_t rt = (int64_t)get_signed(cpu, p->rt);
  int64_t result = rs * rt;
  set(cpu, HI, (uint32_t)((result >> 32) & 0xFFFFFFFF));
  set(cpu, LO, (uint32_t)(result & 0xFFFFFFFF));
}

static void MULTU(CPU* cpu, RParams* p) {
  uint64_t rs = (uint64_t)get(cpu, p->rs);
  uint64_t rt = (uint64_t)get(cpu, p->rt);
  uint64_t result = rs * rt;
  set(cpu, HI, (uint32_t)((result >> 32) & 0xFFFFFFFF));
  set(cpu, LO, (uint32_t)(result & 0xFFFFFFFF));
}

static void NOR(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, ~(get(cpu, p->rs) | get(cpu, p->rt)));
}

static void OR(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) | get(cpu, p->rt));
}

static void ORI(CPU* cpu, struct IParams* p) {
  set(cpu, p->rt, get(cpu, p->rs) | (uint32_t)(p->immediate));
}

static void SLL(CPU* cpu, RParams* p) {
  set(cpu, p->rd, get(cpu, p->rt) << p->shamt);
}

static void SLLV(CPU* cpu, RParams* p) {
  uint32_t shamt = get(cpu, p->rs) & 0x1F;
  set(cpu, p->rd, get(cpu, p->rt) << shamt);
}

static void SRA(CPU* cpu, RParams* p) {
  int32_t value = (int32_t)get(cpu, p->rt);
  set(cpu, p->rd, (uint32_t)(value >> p->shamt));
}

static void SRAV(CPU* cpu, RParams* p) {
  int32_t value = (int32_t)get(cpu, p->rt);
  uint32_t shamt = get(cpu, p->rs) & 0x1F;
  set(cpu, p->rd, (uint32_t)(value >> shamt));
}

static void SRL(CPU* cpu, RParams* p) {
  set(cpu, p->rd, get(cpu, p->rt) >> p->shamt);
}

static void SRLV(CPU* cpu, RParams* p) {
  uint32_t shamt = get(cpu, p->rs) & 0x1F;
  set(cpu, p->rd, get(cpu, p->rt) >> shamt);
}

static void SUB(CPU* cpu, struct RParams* p) {
  int32_t rs = get_signed(cpu, p->rs);
  int32_t rt = get_signed(cpu, p->rt);
  int32_t s = rs - rt;

  if ((rs > 0 && rt < 0 && s < 0) || (rs < 0 && rt > 0 && s > 0)) {
    throw(cpu, ARITHMETIC_OVERFLOW);
  } else {
    set(cpu, p->rd, (uint32_t)s);
  }
}

static void SUBU(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) - get(cpu, p->rt));
}

static void XOR(CPU* cpu, struct RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) ^ get(cpu, p->rt));
}

static void XORI(CPU* cpu, struct IParams* p) {
  set(cpu, p->rt, get(cpu, p->rs) ^ (uint32_t)(p->immediate));
}

static void LUI(CPU* cpu, IParams* p) {
  set(cpu, p->rt, ((uint32_t)p->immediate) << 16);
}

static void SLT(CPU* cpu, RParams* p) {
  set(cpu, p->rd,
      (int32_t)get_signed(cpu, p->rs) < (int32_t)get_signed(cpu, p->rt));
}

static void SLTU(CPU* cpu, RParams* p) {
  set(cpu, p->rd, get(cpu, p->rs) < get(cpu, p->rt));
}

static void SLTI(CPU* cpu, IParams* p) {
  int32_t rs = get_signed(cpu, p->rs);
  int32_t imm = (int16_t)(p->immediate);
  set(cpu, p->rt, rs < imm);
}

static void SLTIU(CPU* cpu, IParams* p) {
  uint32_t rs = get(cpu, p->rs);
  uint32_t imm = (uint16_t)(p->immediate);
  set(cpu, p->rt, rs < imm);
}

static void _branch(CPU* cpu, bool condition, IParams* p) {
  if (condition) {
    int16_t offset = (int16_t)p->immediate;
    cpu->pc = cpu->pc + 4 + (offset * 4);
  } else {
    cpu->pc += 4;
  }
}

static void BEQ(CPU* cpu, IParams* p) {
  _branch(cpu, get(cpu, p->rs) == get(cpu, p->rt), p);
}

static void BNE(CPU* cpu, IParams* p) {
  _branch(cpu, get(cpu, p->rs) != get(cpu, p->rt), p);
}

static void BLEZ(CPU* cpu, IParams* p) {
  _branch(cpu, get_signed(cpu, p->rs) <= 0, p);
}

static void BGTZ(CPU* cpu, IParams* p) {
  _branch(cpu, get_signed(cpu, p->rs) > 0, p);
}

static void BLTZ(CPU* cpu, IParams* p) {
  _branch(cpu, get_signed(cpu, p->rs) < 0, p);
}

static void BGEZ(CPU* cpu, IParams* p) {
  _branch(cpu, get_signed(cpu, p->rs) >= 0, p);
}

static void BLTZ_or_BGEZ(CPU* cpu, IParams* p) {
  if (p->rt == 1) {
    BGEZ(cpu, p);
  } else if (p->rt == 0) {
    BLTZ(cpu, p);
  } else {
    throw(cpu, UNKNOWN_INSTRUCTION);
  }
}

static void J(CPU* cpu, JParams* p) { cpu->pc = p->address << 2; }

static void JAL(CPU* cpu, JParams* p) {
  set(cpu, 31, cpu->pc + 4);
  cpu->pc = p->address << 2;
}

static void JALR(CPU* cpu, RParams* p) {
  uint32_t return_address = cpu->pc + 4;
  cpu->pc = get(cpu, p->rs);
  set(cpu, p->rd, return_address);
}

static void JR(CPU* cpu, RParams* p) { cpu->pc = get(cpu, p->rs); }

static void SYSCALL(CPU* cpu, RParams* p) {
  (void)p;  // Unused parameter

  uint32_t code = get(cpu, 2);  // $v0
  switch (code) {
    case 10:             // exit
      cpu->pc = 0xFFFB;  // Will become 0xFFFF after pc += 4 in run_program
      break;
    case 1:                              // print signed
      printf("%d", get_signed(cpu, 4));  // $a0
      break;
    case 34:                        // print hex
      printf("%08X", get(cpu, 4));  // $a0
      break;
    case 35:  // print binary
    {
      uint32_t value = get(cpu, 4);  // $a0
      for (int i = 31; i >= 0; i--) {
        putchar((value & (1U << i)) ? '1' : '0');
      }
      break;
    }
    case 36:                      // print unsigned
      printf("%u", get(cpu, 4));  // $a0
      break;
    case 11:                              // print char
      printf("%c", (char)(get(cpu, 4)));  // $a0
      break;
    case 4:  // print string
    {
      uint32_t address = get(cpu, 4);  // $a0
      while (true) {
        uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
        uint32_t word = get_memory(cpu, address - byte_offset);

        // Check for memory error
        if (cpu->error != NO_ERROR) {
          return;
        }

        char byte = (word >> (8 * (3 - byte_offset))) & 0xFF;
        if (byte == '\0') {
          break;
        }
        printf("%c", byte);
        address++;
      }
      break;
    }
    default:
      printf("Unknown syscall code: %u\n", code);
      throw(cpu, UNKNOWN_SYSCALL);
  }
}

static void SB(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t mask = ((uint32_t)0xFF) << (8 * (3 - byte_offset));

  uint32_t word = get_memory(cpu, address - byte_offset);

  set_memory(
      cpu, address - byte_offset,
      (word & ~mask) | ((get(cpu, p->rt) & 0xFFU) << (8 * (3 - byte_offset))));
}

static void SH(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  if (address % 2 != 0) {
    throw(cpu, MEMORY_ALIGNMENT);
    return;
  }
  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t mask = 0xFFFFU << (8 * (2 - byte_offset));
  uint32_t word = get_memory(cpu, address - byte_offset);

  set_memory(cpu, address - byte_offset,
             (word & ~mask) |
                 ((get(cpu, p->rt) & 0xFFFFU) << (8 * (2 - byte_offset))));
}

static void SW(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  if (address % 4 != 0) {
    throw(cpu, MEMORY_ALIGNMENT);
    return;
  }
  set_memory(cpu, address, get(cpu, p->rt));
}

static void LB(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t word = get_memory(cpu, address - byte_offset);
  uint8_t byte = (word >> (8 * (3 - byte_offset))) & 0xFF;

  // Sign-extend
  if (byte & 0x80) {
    set(cpu, p->rt, (uint32_t)(int32_t)(int8_t)byte);
  } else {
    set(cpu, p->rt, (uint32_t)byte);
  }
}

static void LBU(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t word = get_memory(cpu, address - byte_offset);
  uint8_t byte = (word >> (8 * (3 - byte_offset))) & 0xFF;
  set(cpu, p->rt, (uint32_t)byte);
}

static void LH(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  if (address % 2 != 0) {
    throw(cpu, MEMORY_ALIGNMENT);
    return;
  }

  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t word = get_memory(cpu, address - byte_offset);
  uint16_t halfword = (word >> (8 * (2 - byte_offset))) & 0xFFFF;
  // Sign-extend
  if (halfword & 0x8000) {
    set(cpu, p->rt, (uint32_t)(int32_t)(int16_t)halfword);
  } else {
    set(cpu, p->rt, (uint32_t)halfword);
  }
}

static void LHU(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  if (address % 2 != 0) {
    throw(cpu, MEMORY_ALIGNMENT);
    return;
  }

  uint32_t byte_offset = address % 4;  // 0 = highest byte, 3 = lowest
  uint32_t word = get_memory(cpu, address - byte_offset);
  uint16_t halfword = (word >> (8 * (2 - byte_offset))) & 0xFFFF;
  set(cpu, p->rt, (uint32_t)halfword);
}

static void LW(CPU* cpu, IParams* p) {
  uint32_t address = get(cpu, p->rs) + (int16_t)(p->immediate);
  if (address % 4 != 0) {
    throw(cpu, MEMORY_ALIGNMENT);
    return;
  }
  set(cpu, p->rt, get_memory(cpu, address));
}

static void MFHI(CPU* cpu, RParams* p) { set(cpu, p->rd, get(cpu, HI)); }

static void MFLO(CPU* cpu, RParams* p) { set(cpu, p->rd, get(cpu, LO)); }

static void MTHI(CPU* cpu, RParams* p) { set(cpu, HI, get(cpu, p->rs)); }

static void MTLO(CPU* cpu, RParams* p) { set(cpu, LO, get(cpu, p->rs)); }

const char* ERROR_STRINGS[] = {"No Error",         "Memory Alignment Error",
                               "PC Out of Bounds", "Arithmetic Overflow",
                               "Divide by Zero",   "Unknown Instruction",
                               "Unknown Syscall",  "Memory Out of Bounds"};

/* Use runtime initialization instead of designated initializers to avoid
   C dialect issues (some compilers or build configs do not accept
   the [index] = { ... } syntax). */
static RInstruction R_INSTRUCTIONS[64];
static IInstruction I_INSTRUCTIONS[64];
static JInstruction J_INSTRUCTIONS[64];

static void init_instructions() {
  /* Clear all entries first */
  for (int i = 0; i < 64; i++) {
    R_INSTRUCTIONS[i].execute = NULL;
    R_INSTRUCTIONS[i].inc_pc = false;
    R_INSTRUCTIONS[i].funct = 0;
    I_INSTRUCTIONS[i].execute = NULL;
    I_INSTRUCTIONS[i].inc_pc = false;
    I_INSTRUCTIONS[i].opcode = 0;
    J_INSTRUCTIONS[i].execute = NULL;
    J_INSTRUCTIONS[i].opcode = 0;
  }

  /* Populate R instructions */
  R_INSTRUCTIONS[ADD_OP] = (RInstruction){true, ADD_OP, ADD};
  R_INSTRUCTIONS[ADDU_OP] = (RInstruction){true, ADDU_OP, ADDU};
  R_INSTRUCTIONS[AND_OP] = (RInstruction){true, AND_OP, AND};
  R_INSTRUCTIONS[DIV_OP] = (RInstruction){true, DIV_OP, DIV};
  R_INSTRUCTIONS[DIVU_OP] = (RInstruction){true, DIVU_OP, DIVU};
  R_INSTRUCTIONS[MULT_OP] = (RInstruction){true, MULT_OP, MULT};
  R_INSTRUCTIONS[MULTU_OP] = (RInstruction){true, MULTU_OP, MULTU};
  R_INSTRUCTIONS[NOR_OP] = (RInstruction){true, NOR_OP, NOR};
  R_INSTRUCTIONS[OR_OP] = (RInstruction){true, OR_OP, OR};
  R_INSTRUCTIONS[SLL_OP] = (RInstruction){true, SLL_OP, SLL};
  R_INSTRUCTIONS[SLLV_OP] = (RInstruction){true, SLLV_OP, SLLV};
  R_INSTRUCTIONS[SRA_OP] = (RInstruction){true, SRA_OP, SRA};
  R_INSTRUCTIONS[SRAV_OP] = (RInstruction){true, SRAV_OP, SRAV};
  R_INSTRUCTIONS[SRL_OP] = (RInstruction){true, SRL_OP, SRL};
  R_INSTRUCTIONS[SRLV_OP] = (RInstruction){true, SRLV_OP, SRLV};
  R_INSTRUCTIONS[SUB_OP] = (RInstruction){true, SUB_OP, SUB};
  R_INSTRUCTIONS[SUBU_OP] = (RInstruction){true, SUBU_OP, SUBU};
  R_INSTRUCTIONS[XOR_OP] = (RInstruction){true, XOR_OP, XOR};
  R_INSTRUCTIONS[SLT_OP] = (RInstruction){true, SLT_OP, SLT};
  R_INSTRUCTIONS[SLTU_OP] = (RInstruction){true, SLTU_OP, SLTU};
  R_INSTRUCTIONS[JALR_OP] = (RInstruction){false, JALR_OP, JALR};
  R_INSTRUCTIONS[JR_OP] = (RInstruction){false, JR_OP, JR};
  R_INSTRUCTIONS[SYSCALL_OP] = (RInstruction){true, SYSCALL_OP, SYSCALL};
  R_INSTRUCTIONS[MFHI_OP] = (RInstruction){true, MFHI_OP, MFHI};
  R_INSTRUCTIONS[MFLO_OP] = (RInstruction){true, MFLO_OP, MFLO};
  R_INSTRUCTIONS[MTHI_OP] = (RInstruction){true, MTHI_OP, MTHI};
  R_INSTRUCTIONS[MTLO_OP] = (RInstruction){true, MTLO_OP, MTLO};

  /* Populate I instructions */
  I_INSTRUCTIONS[ADDI_OP] = (IInstruction){true, ADDI_OP, ADDI};
  I_INSTRUCTIONS[ADDIU_OP] = (IInstruction){true, ADDIU_OP, ADDIU};
  I_INSTRUCTIONS[ANDI_OP] = (IInstruction){true, ANDI_OP, ANDI};
  I_INSTRUCTIONS[LUI_OP] = (IInstruction){true, LUI_OP, LUI};
  I_INSTRUCTIONS[ORI_OP] = (IInstruction){true, ORI_OP, ORI};
  I_INSTRUCTIONS[SLTI_OP] = (IInstruction){true, SLTI_OP, SLTI};
  I_INSTRUCTIONS[SLTIU_OP] = (IInstruction){true, SLTIU_OP, SLTIU};
  I_INSTRUCTIONS[XORI_OP] = (IInstruction){true, XORI_OP, XORI};
  I_INSTRUCTIONS[BEQ_OP] = (IInstruction){false, BEQ_OP, BEQ};
  I_INSTRUCTIONS[BNE_OP] = (IInstruction){false, BNE_OP, BNE};
  I_INSTRUCTIONS[BLEZ_OP] = (IInstruction){false, BLEZ_OP, BLEZ};
  I_INSTRUCTIONS[BGTZ_OP] = (IInstruction){false, BGTZ_OP, BGTZ};
  I_INSTRUCTIONS[BLTZ_or_BGEZ_OP] =
      (IInstruction){false, BLTZ_or_BGEZ_OP, BLTZ_or_BGEZ};
  I_INSTRUCTIONS[SB_OP] = (IInstruction){true, SB_OP, SB};
  I_INSTRUCTIONS[SH_OP] = (IInstruction){true, SH_OP, SH};
  I_INSTRUCTIONS[SW_OP] = (IInstruction){true, SW_OP, SW};
  I_INSTRUCTIONS[LB_OP] = (IInstruction){true, LB_OP, LB};
  I_INSTRUCTIONS[LBU_OP] = (IInstruction){true, LBU_OP, LBU};
  I_INSTRUCTIONS[LH_OP] = (IInstruction){true, LH_OP, LH};
  I_INSTRUCTIONS[LHU_OP] = (IInstruction){true, LHU_OP, LHU};
  I_INSTRUCTIONS[LW_OP] = (IInstruction){true, LW_OP, LW};

  /* Populate J instructions */
  J_INSTRUCTIONS[J_OP] = (JInstruction){J_OP, J};
  J_INSTRUCTIONS[JAL_OP] = (JInstruction){JAL_OP, JAL};
}

CPU* CPU_init() {
  CPU* cpu = (CPU*)malloc(sizeof(CPU));
  // Initialize all registers and memory to zero
  for (int i = 0; i < 34; i++) {
    cpu->regs[i] = 0;
  }
  for (int i = 0; i < MEMORY_SIZE / 4; i++) {
    cpu->memory[i] = 0;
  }
  for (int i = 0; i < STACK_SIZE / 4; i++) {
    cpu->stack[i] = 0;
  }
  for (int i = 0; i < PROGRAM_SIZE; i++) {
    cpu->program[i] = 0;
  }
  cpu->program_size = 0;
  cpu->pc = 0;
  cpu->error = NO_ERROR;
  cpu->error_address = 0;

  // Initialize $sp to top of stack
  cpu->regs[29] = STACK_START_ADDRESS;
  // Set $ra to 0xFFFF (as program end sentinel)
  cpu->regs[31] = 0xFFFF;

  /* Initialize instruction tables (avoids use of designated initializers
     which may not be supported by all C dialects used to build this code). */
  init_instructions();

  return cpu;
}

void execute_instruction(CPU* cpu, uint32_t instr) {
  uint8_t opcode = (instr >> 26) & 0x3F;

  // instr is R-format
  if (opcode == 0) {
    uint8_t funct = instr & 0x3F;
    RParams params;
    params.rs = (instr >> 21) & 0x1F;
    params.rt = (instr >> 16) & 0x1F;
    params.rd = (instr >> 11) & 0x1F;
    params.shamt = (instr >> 6) & 0x1F;

    RInstruction r_instr = R_INSTRUCTIONS[funct];

    if (r_instr.execute == NULL) {
      throw(cpu, UNKNOWN_INSTRUCTION);
    } else {
      r_instr.execute(cpu, &params);
      if (r_instr.inc_pc) {
        cpu->pc += 4;
      }
    }
  }
  // instr is J-format
  else if (opcode == 0b000011 || opcode == 0b000010) {
    JParams params;
    params.address = instr & 0x03FFFFFF;

    JInstruction j_instr = J_INSTRUCTIONS[opcode];

    if (j_instr.execute == NULL) {
      throw(cpu, UNKNOWN_INSTRUCTION);
    } else {
      j_instr.execute(cpu, &params);
    }
  }
  // instr is I-format
  else {
    IParams params;
    params.rs = (instr >> 21) & 0x1F;
    params.rt = (instr >> 16) & 0x1F;
    params.immediate = instr & 0xFFFF;

    IInstruction i_instr = I_INSTRUCTIONS[opcode];

    if (i_instr.execute == NULL) {
      throw(cpu, UNKNOWN_INSTRUCTION);
    } else {
      i_instr.execute(cpu, &params);
      if (i_instr.inc_pc) {
        cpu->pc += 4;
      }
    }
  }
}

void run_program(CPU* cpu) {
  while (cpu->error == NO_ERROR && cpu->pc != 0xFFFF) {
    if (cpu->pc % 4 != 0) {
      throw(cpu, MEMORY_ALIGNMENT);
      break;
    }
    if (cpu->pc / 4 >= cpu->program_size) {
      throw(cpu, PROGRAM_OUT_OF_BOUNDS);
      break;
    }
    uint32_t instr = cpu->program[cpu->pc / 4];
    execute_instruction(cpu, instr);
  }
  if (cpu->error != NO_ERROR) {
    fprintf(stderr, "Error occurred at address: 0x%04X\n", cpu->error_address);
    fprintf(stderr, "Error: %s\n", ERROR_STRINGS[cpu->error]);
  }
}

CPU* load_program(char* filename) {
  CPU* cpu = CPU_init();

  FILE* file = fopen(filename, "r");
  if (file == NULL) {
    fprintf(stderr, "Error: Could not open file %s\n", filename);
    return NULL;
  }

  char line[34];  // 32 bit chunks + newline + null terminator
  uint16_t program_size = 0;
  while (fgets(line, sizeof(line), file) != NULL) {
    if (program_size >= PROGRAM_SIZE) {
      fprintf(stderr, "Error: Program size exceeds maximum limit of %d\n",
              PROGRAM_SIZE);
      fclose(file);
      CPU_destroy(cpu);
      return NULL;
    }
    cpu->program[program_size] = (uint32_t)strtol(line, NULL, 2);
    program_size++;
  }
  cpu->program_size = program_size;
  fclose(file);

  return cpu;
}
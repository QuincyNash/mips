#include "opcodes.h"

#include <stdint.h>

const uint8_t ADD_OP = 0b100000;
const uint8_t ADDU_OP = 0b100001;
const uint8_t AND_OP = 0b100100;
const uint8_t DIV_OP = 0b011010;
const uint8_t DIVU_OP = 0b011011;
const uint8_t MULT_OP = 0b011000;
const uint8_t MULTU_OP = 0b011001;
const uint8_t NOR_OP = 0b100111;
const uint8_t OR_OP = 0b100101;
const uint8_t SLL_OP = 0b000000;
const uint8_t SLLV_OP = 0b000100;
const uint8_t SRA_OP = 0b000011;
const uint8_t SRAV_OP = 0b000111;
const uint8_t SRL_OP = 0b000010;
const uint8_t SRLV_OP = 0b000110;
const uint8_t SUB_OP = 0b100010;
const uint8_t SUBU_OP = 0b100011;
const uint8_t XOR_OP = 0b100110;
const uint8_t SLT_OP = 0b101010;
const uint8_t SLTU_OP = 0b101011;
const uint8_t JALR_OP = 0b001001;
const uint8_t JR_OP = 0b001000;
const uint8_t SYSCALL_OP = 0b001100;
const uint8_t MFHI_OP = 0b010000;
const uint8_t MFLO_OP = 0b010010;
const uint8_t MTHI_OP = 0b010001;
const uint8_t MTLO_OP = 0b010011;

const uint8_t ADDI_OP = 0b001000;
const uint8_t ADDIU_OP = 0b001001;
const uint8_t ANDI_OP = 0b001100;
const uint8_t LUI_OP = 0b001111;
const uint8_t ORI_OP = 0b001101;
const uint8_t SLTI_OP = 0b001010;
const uint8_t SLTIU_OP = 0b001011;
const uint8_t XORI_OP = 0b001110;
const uint8_t BEQ_OP = 0b000100;
const uint8_t BNE_OP = 0b000101;
const uint8_t BLEZ_OP = 0b000110;
const uint8_t BGTZ_OP = 0b000111;
const uint8_t BLTZ_or_BGEZ_OP = 0b000001;
const uint8_t SB_OP = 0b101000;
const uint8_t SH_OP = 0b101001;
const uint8_t SW_OP = 0b101011;
const uint8_t LB_OP = 0b100000;
const uint8_t LBU_OP = 0b100100;
const uint8_t LH_OP = 0b100001;
const uint8_t LHU_OP = 0b100101;
const uint8_t LW_OP = 0b100011;

const uint8_t J_OP = 0b000010;
const uint8_t JAL_OP = 0b000011;

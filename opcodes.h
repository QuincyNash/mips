#ifndef _OPCODES_H_
#define _OPCODES_H_

#include <stdint.h>

extern const uint8_t ADD_OP;
extern const uint8_t ADDU_OP;
extern const uint8_t AND_OP;
extern const uint8_t ADD_OP;
extern const uint8_t ADDU_OP;
extern const uint8_t AND_OP;
extern const uint8_t DIV_OP;
extern const uint8_t DIVU_OP;
extern const uint8_t MULT_OP;
extern const uint8_t MULTU_OP;
extern const uint8_t NOR_OP;
extern const uint8_t OR_OP;
extern const uint8_t SLL_OP;
extern const uint8_t SLLV_OP;
extern const uint8_t SRA_OP;
extern const uint8_t SRAV_OP;
extern const uint8_t SRL_OP;
extern const uint8_t SRLV_OP;
extern const uint8_t SUB_OP;
extern const uint8_t SUBU_OP;
extern const uint8_t XOR_OP;
extern const uint8_t SLT_OP;
extern const uint8_t SLTU_OP;
extern const uint8_t JALR_OP;
extern const uint8_t JR_OP;
extern const uint8_t SYSCALL_OP;
extern const uint8_t MFHI_OP;
extern const uint8_t MFLO_OP;
extern const uint8_t MTHI_OP;
extern const uint8_t MTLO_OP;

extern const uint8_t ADDI_OP;
extern const uint8_t ADDIU_OP;
extern const uint8_t ANDI_OP;
extern const uint8_t LUI_OP;
extern const uint8_t ORI_OP;
extern const uint8_t SLTI_OP;
extern const uint8_t SLTIU_OP;
extern const uint8_t XORI_OP;
extern const uint8_t BEQ_OP;
extern const uint8_t BNE_OP;
extern const uint8_t BLEZ_OP;
extern const uint8_t BGTZ_OP;
extern const uint8_t BLTZ_or_BGEZ_OP;
extern const uint8_t SB_OP;
extern const uint8_t SH_OP;
extern const uint8_t SW_OP;
extern const uint8_t LB_OP;
extern const uint8_t LBU_OP;
extern const uint8_t LH_OP;
extern const uint8_t LHU_OP;
extern const uint8_t LW_OP;

extern const uint8_t J_OP;
extern const uint8_t JAL_OP;

#endif
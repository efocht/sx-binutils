/* Opcodes for NEC SX machines.

   Copyright 2007, 2008 NEC HPCE

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.

   Authors: Erich Focht <efocht@hpce.nec.com>
            jaKa Mocnik <jaka@xlab.si>
            Marko Novak <marko.novak@xlab.si>
            Matthias Hess
*/

#ifndef __OPCODES_SX_H__
#define __OPCODES_SX_H__


/* Bit flags to indicate permitted architecture of opcode */

#define _sx_undef  0x0000
#define sx4        0x0001
#define sx5        0x0002
#define sx6        0x0004
#define sx8        0x0008
#define sx8r       0x0010


/* Aliases */

#define sx8_up     (sx8 | sx8r)
#define sx6_up     (sx6 | sx8_up)
#define sx5_up     (sx5 | sx6_up)
#define sx4_up     (sx4 | sx5_up)

typedef struct sx_opcode {
	char *name;		/* mnemonic */
	unsigned char opcode;   /* opcode 1 byte */
	short format;	        /* instruction and opcode format */
	unsigned short flag;  	/* suffixes and special flags */
	struct {
		unsigned short x;  /* x-field check flags */
		unsigned short y;  /* y-field check flags */
		unsigned short z;  /* z-field check flags */
		unsigned short d;  /* d-field check flags */
	} field;
	struct {
		char x;            /* fixed vreg x-field */
		char y;            /* fixed vreg y-field */
		char z;            /* fixed vreg z-field */
	} vreg;
} sx_opcode;


/* SX instruction fields in instructions */
enum sx_ins_field {
    SX_FIELD_X,
    SX_FIELD_Y,
    SX_FIELD_Z,
    SX_FIELD_D
};

/* This is where the list of opcodes lives ... */
extern const struct sx_opcode sx_opcodes[];
extern const int sx_num_opcodes;
extern char *sx_fixed_vreg[];
extern char *sx_fixed_vreg2[];

/* index of an opcode inside the opcodes table */
#define sx_opcode_idx(op)                                           \
	(((char *)(op) - (char *)(&sx_opcodes[0])) / sizeof(sx_opcode))

/*
 * Find opcode entry corresponding to the passed opcode byte.
 */
static inline int sx_find_opc_idx(char opc)
{
	int i, idx = -1;
    
	for (i = 0; i <= sx_num_opcodes; i++)
		if (sx_opcodes[i].opcode == opc) {
			idx = i;
			break;
		}
	return idx;
}

/*
 * SX instructions are either 4 or 8 bytes long.
 * The opcode is in the first byte, qualifiers and field details follow.
 * The SX is a big endian machine, therefore loading the instructions
 * into the structure depends on the host machine's endianness.
 */
typedef struct {
	union {
		struct {
			unsigned char op;
			unsigned char x;
			unsigned char y;
			unsigned char z;
		} w1;
		unsigned int w2;
		unsigned char c[4];
	} opc;
	int d;
} sx_instr;

#if 0
/*
 * Miscellaneous registers for SMIR and LMIR commands.
 *
 *
 */
enum misc_reg_idx {
	SX_MIR_VECC = 0, //00000: Vector execution clock counter
	SX_MIR_FPEC,     //00001: Floating-point data execution element counter
	SX_MIR_CMCC,     //00010: Cache miss clock counter
	SX_MIR_BCCC,     //00011: Bank conflict clock counter
	SX_MIR_DIDR,     //00100: Debugging identifier register
	SX_MIR_SAR,      //00101: Store address register
	SX_MIR_ICMCC,    //00110: Instruction cache miss clock counter
	SX_MIR_SIDR,     //00111: Security identifier register
	SX_MIR_BPFC,     //01000: Branch prediction failure counter
	SX_MIR_USRCC,    //01001: User clock counter
	SX_MIR_IPHCC,    //01010: Instruction pipeline hold clock counter
	SX_MIR_SPEC,     //01011: Special (reserved for future use?)
	SX_MIR_VAREC,    //01100: Vector arithm. execution clock counter
	SX_MIR_VLDEC,    //01101: Vector load execution clock counter
	SX_MIR_MNUBC,    //01110: Memory network unit b* counter
	SX_MIR_SRACC,    //01111
	SX_MIR_MIDR0,    //10000
	SX_MIR_MIDR1,    //10001
	SX_MIR_MIDR2,    //10010
	SX_MIR_MIDR3,    //10011
	SX_MIR_RFU1,     //10100
	SX_MIR_RFU2,     //10101
	SX_MIR_RFU3,     //10110
	SX_MIR_RFU4,     //10111
	SX_MIR_PMMR,     //11000
	SX_MIR_RFU5      //11001
};

static char *mirtab[] = {
	[SX_MIR_VECC] = "vecc",
	[SX_MIR_FPEC] = "fpec",
	[SX_MIR_CMCC] = "cmcc",
	[SX_MIR_BCCC] = "bccc",
	[SX_MIR_DIDR] = "didr",
	[SX_MIR_SAR]  = "sar",
	[SX_MIR_ICMCC]= "icmcc",
	[SX_MIR_SIDR] = "sidr",
	[SX_MIR_BPFC] = "bpfc",
	[SX_MIR_USRCC]= "usrcc",
	[SX_MIR_IPHCC]= "iphcc",
	[SX_MIR_SPEC] = "spec",
	[SX_MIR_VAREC]= "varec",
	[SX_MIR_VLDEC]= "vldec",
	[SX_MIR_MNUBC]= "mnubc",
	[SX_MIR_SRACC]= "sracc",
	[SX_MIR_MIDR0]= "midr0",
	[SX_MIR_MIDR1]= "midr1",
	[SX_MIR_MIDR2]= "midr2",
	[SX_MIR_MIDR3]= "midr3",
	[SX_MIR_RFU1] = "rfu1",
	[SX_MIR_RFU2] = "rfu2",
	[SX_MIR_RFU3] = "rfu3",
	[SX_MIR_RFU4] = "rfu4",
	[SX_MIR_PMMR] = "pmmr",
	[SX_MIR_RFU5] = "rfu5"
};

enum spec_reg_idx {
	SX_SR_STM = 0,
	SX_SR_ITM,
	SX_SR_GTM,
	SX_SR_GTM2
};

static char *sr_tab[] = {
	[SX_SR_STM] = "stm",
	[SX_SR_ITM] = "itm",
	[SX_SR_GTM] = "gtm",
	[SX_SR_GTM2]= "gtm2"
};
#endif

/* ================================= */

#define O_BPT     0x3F020000    /* Break point */
#ifdef PDBX
#   define O_BPTM    0x3F800000	/* mutitask Break point instruction */
#endif
typedef unsigned char Opcode;

#define O_DUMMY00 0x00	/* noop */
#define O_LDS   0x01	/* load 8 byte word */
#define O_LDU   0x02	/* load scalar register upper (0-31) bits */
#define O_LDL   0x03	/* load scalar register lower (32-63) bits */
#define O_LD2B  0x04	/* load 2 byte (short) with sign extension */
#define O_LD1B  0x05	/* load 1 byte char */
#define O_LEA   0x06	/* load/link effective address */
#define O_TLA   0x07	/* ?? unknown/undocumented */
#define O_BSIC  0x08	/* branch and save instruction counter (IC) */
#define O_DLDS	0x09	/* dismissable load scalar register (8 byte) (no exception if fails) */
#define O_DLDU	0x0A	/* dismissable load scalar upper (0-31) bits (no exceptions) */
#define O_DLDL	0x0B	/* dismissable load scalar lower (32-63) bits (no exceptions) */
#define O_DUMMY0C 0x0C	/* not used, unknown */
#define O_DUMMY0D 0x0D	/* not used, unknown */
#define O_LDM   0x0E	/* load multiple scalar registers */
#define O_CVD	0x0F	/* convert to double format (single precision -> double precision IEEE */
#define O_DUMMY10 0x10	/* not used, unknown */
#define O_STS   0x11	/* store scalar register */
#define O_STU   0x12	/* store scalar register upper (0-31) bits */
#define O_STL   0x13	/* store scalar register lower(32-63) bits */
#define O_ST2B  0x14	/* store 2 byte short */
#define O_ST1B  0x15	/* store 1 byte char */
#define O_DUMMY16 0x16	/* not used, unknown */
#define O_DUMMY17 0x17	/* not used, unknown */
#define O_DUMMY18 0x18	/* not used, unknown */
#define O_BC    0x19	/* branch on condition (many variants available!) */
#define O_DUMMY1A 0x1A	/* not used, unknown */
#define O_BCS   0x1B	/* branch on condition single (bits 32-63 of imm val are compared)*/
#define O_BCF   0x1C	/* branch on condition floating point */
#define O_DUMMY1D 0x1D	/* not used, unknown */
#define O_STM   0x1E	/* store multiple scalar registers */
#define O_CVS	0x1F	/* convert to single format (dp float IEEE -> sp float IEEE) */
#define O_LCR   0x20	/* load communication register */
#define O_LAS   0x21	/* load from absolute address (physical addressing) */
#define O_SMIR  0x22	/* store miscellaneous register (many variants for many registers) */
#define O_LAL   0x23	/* load from absolute address, lower bits (32-63) */
#define O_SVT   0x24	/* save timer register */
#define O_SEX   0x25	/* save execution counter (how boring!) */
#define O_SVX   0x26	/* save vector execution counter */
#define O_SVE   0x27	/* save vector element counter */
#define O_SIC   0x28	/* save instruction counter */
#define O_SFR   0x29	/* save flag register */
#define O_SPM   0x2A	/* save program mode flags */
#define O_SSM   0x2B	/* save system mask (priviledged) */
#define O_SPSW  0x2C	/* save process status word */
#define O_RPN   0x2D	/* read processor number (priv) */
#define O_SMVL  0x2E	/* save maximum vector length */
#define O_SVL   0x2F	/* save vector length (VL) register */
#define O_SCR   0x30	/* save communication register */
#define O_SAS   0x31	/* store to absolute address (physicall addressing mode) */
#define O_LMIR  0x32	/* load miscellaneous register (many variants available) */
#define O_SAL   0x33	/* store to absolute address lower */
#define O_LDT   0x34	/* load timer register (priv) */
#define O_LEX   0x35	/* load execution counter */
#define O_LVX   0x36	/* load vector execution counter (priv) */
#define O_LVE   0x37	/* load vector element counter (priv) */
#define O_INH   0x38	/* inhibit interrupts (priv) */
#define O_ENI   0x39	/* enable interrupts (priv) */
#define O_LPM   0x3A	/* load program mode flags */
#define O_LSM   0x3B	/* load system mask (priv) */
#define O_RCR   0x3C	/* reset control registers */
#define O_DIAG  0x3D	/* diagnose (priv) */
#define O_WAIT  0x3E	/* wait (priv), CPU halts and waits for an interrupt */
#define O_MONC  0x3F	/* monitor call (generates program exception, high speed debugging) */
#define O_TSCR  0x40	/* test and set communication register */
#define O_SCRNS 0x41	/* store comm register without synchronization */
#define O_TS1AM 0x42	/* test and set one 8 byte word, one bit controls each memory byte */
#define O_TS2AM 0x43	/* like ts1am, but with additional condition that memory byte is zero */
#define O_AND   0x44	/* AND */
#define O_OR    0x45	/* OR */
#define O_XOR   0x46	/* exclusive OR */
#define O_EQV   0x47	/* Equivalence */
#define O_ADD   0x48	/* unsigned add 64bit */
#define O_MPY   0x49	/* multiply 64bit */
#define O_ADS   0x4A	/* add single to lower 32bit */
#define O_MPS   0x4B	/* multiply single to lower 32bit*/
#define O_FDA   0x4C	/* floating point scalar add */
#define O_FMP   0x4D	/* floating point scalar multiply */
#define O_FIX   0x4E	/* convert float to fixed point (int) (in lower 32 bits) */
#define O_FIXX  0x4F	/* convert dp float to fixed point (64 bit) long */
#define O_LDCL  0x50	/* load and clear: load scalar register and clear memory */
#define O_SCM   0x51	/* unknown, undocumented */
#define O_TS3AM 0x52	/* test and set, more complex one than ts2am */
#define O_ATMAM 0x53	/* atomic arithmetic manipulation */
#define O_NND   0x54	/* negating AND : Sx <- (!Sy) & Sz */
#define O_CMP   0x55	/* compare (scalar) */
#define O_MRG   0x56	/* merge (bit by bit): Sx <- (Sx & (!Sz)) | (Sy & Sz) (I think) */
#define O_SLAX  0x57	/* shift left arithmetic (lower order six bits are shifted to the left) */
#define O_SUB   0x58	/* unsigned subtract scalar 64 bit */
#define O_ADX   0x59	/* add scalar 64bit */
#define O_SBS   0x5A	/* subtract single scalar lower 32 bits */
#define O_SBX   0x5B	/* subtract scalar 64 bit */
#define O_FSB   0x5C	/* floating point scalar subtract (sp or dp) */
#define O_FDV   0x5D	/* floating point scalar divide */
#define O_FLT   0x5E	/* convert to floating point lower 32 bit*/
#define O_FLTX  0x5F	/* convert to floating point 64 bit */
#define O_FIDCR 0x60	/* fetch and increment/decrement comm register */
#define O_SNPSB 0x61	/* save NPSB address (next PSB CPU register, see interrupt handling) */
#define O_SCPSB 0x62	/* save current PSB address */
#define O_DUMMY63 0x63	/* unknown, undocumented */
#define O_SLD   0x64	/* shift left double */
#define O_SLL   0x65	/* shift left logical */
#define O_SLA   0x66	/* shift left arithmetic lower 32 bit */
#define O_LDZ   0x67	/* loading zero count, load number of leading zero bits */
#define O_RMSG  0x68	/* read message and reset (priv) */
#define O_LFR   0x69	/* load flag register */
#define O_CPX   0x6A	/* compare scalar 64 bit */
#define O_SSR   0x6B	/* save system register */
#define O_FAQ   0x6C	/* floating add quadruple */
#define O_FMQ   0x6D	/* floating multiply quadruple */
#define O_MPX	0x6E	/* multiply 64 bit signed integers */
#define O_DUMMY6F 0x6F	/*  */
#define O_RET   0x70	/* return (from interrupt?)(loads PSW, TM, EX, IC, USRCC from M(CPSB)) */
#define O_LNPSB 0x71	/* load NPSB from scalar register into CPU NPSB register */
#define O_LCPSB 0x72	/* load CPSB from scalar register into CPU CPSB register */
#define O_CHGSP 0x73	/* change partial space (priv), load PSPT into CPU ATB*/
#define O_SRD   0x74	/* shift right double */
#define O_SRL   0x75	/* shift right logical */
#define O_SRA   0x76	/* shift right arithmetic lower 32 bits */
#define O_SRAX  0x77	/* shift right arithmetic */
#define O_SEND  0x78	/* send message (priv) to CPU, IOP or DGP */
#define O_NOP   0x79	/*  */
#define O_CPS   0x7A	/* compare single lower 32 bits */
#define O_LSR   0x7B	/* load system register (priv) */
#define O_FSQ   0x7C	/* floating point subtract quadruple */
#define O_FCQ   0x7D	/* floating point compare quadruple */
#define O_FCP   0x7E	/* floating point compare */
#define O_DUMMY7F 0x7F	/*  */
#define O_VLDX  0x80	/* vector load 64 bit words (can load into vector data register) */
#define O_VLD   0x81	/* vector load */
#define O_VLDU  0x82	/* vector load upper 32 bits */
#define O_VLDL  0x83	/* vector load lower 32 bits */
#define O_ANDM  0x84	/* AND vector masks */
#define O_ORM   0x85	/* OR vector masks */
#define O_XORM  0x86	/* XOR vector masks */
#define O_EQVM  0x87	/* EQV vector masks */
#define O_MVCM  0x88	/* move vector control register (VC) to vector mask */
#define O_VMAXS 0x89	/* vector maximum single */
#define O_LVM   0x8A	/* load vector mask */
#define O_VADX  0x8B	/* vector add (can store to vector data register) */
#define O_VBRD  0x8C	/* vector broadcast scalar register value to vector (data) register */
#define O_VCP   0x8D	/* vector compress */
#define O_LSV   0x8E	/* load scalar register to _one_ vector register element */
#define O_VCVD  0x8F	/* vector convert sp float to dp IEEE float format */
#define O_VLDUX 0x90	/* vector load upper */
#define O_VST   0x91	/* vector store */
#define O_VSTU  0x92	/* vector store upper */
#define O_VSTL  0x93	/* vector store lower */
#define O_NNDM  0x94	/* negating AND vector mask */
#define O_NEGM  0x95	/* negate vector mask */
#define O_DUMMY96 0x96	/*  */
#define O_DUMMY97 0x97	/*  */
#define O_MVMC  0x98	/* move vector mask to VC */
#define O_VMINS 0x99	/* vector minimum single */
#define O_SVM   0x9A	/* save vector mask */
#define O_VSBX  0x9B	/* vector subtract (can store to VD) */
#define O_VMV   0x9C	/* vector move */
#define O_VEX   0x9D	/* vector expand (oppositoe of compress) */
#define O_LVS   0x9E	/* load _one_ vector element into a scalar register */
#define O_VCVS  0x9F	/* vector convert to single precision format */
#define O_VLDLX 0xA0	/* vector load lower (can store to VD)*/
#define O_VGT   0xA1	/* vector gather */
#define O_VGTU  0xA2	/* vector gather upper */
#define O_VGTL  0xA3	/* vector gather lower */
#define O_DUMMYA4 0xA4	/*  */
#define O_TOVC  0xA5	/* trailing ones VC: count bits in VC up to the last one which is set */
#define O_PCNT  0xA6	/* population count: count set bits in VC */
#define O_LZVC  0xA7	/* leading zeroes in VC */
#define O_VFIXX 0xA8	/* vector convert to fixed point */
#define O_VFSX  0xA9	/* vector convert to fixed point (single precision) */
#define O_VSUMX 0xAA	/* vector sum, store result in first element of vector register */
#define O_VMAXX 0xAB	/* vector maximum, store max value and element number */
#define O_DUMMYAC 0xAC	/*  */
#define O_VFMAX 0xAD	/* vector floating maximum */
#define O_VMAD  0xAE	/* vector move arithm register to vector data register */
#define O_DUMMYAF 0xAF	/*  */
#define O_VSFA  0xB0	/* vector shift left and add */
#define O_VSC   0xB1	/* vector scatter */
#define O_VSCU  0xB2	/* vector scatter upper */
#define O_VSCL  0xB3	/* vector scatter lower */
#define O_VFMK  0xB4	/* vector from mask */
#define O_DUMMYB5 0xB5	/*  */
#define O_VFMS  0xB6	/* vector from mask single */
#define O_VPCNT 0xB7	/* vector population count */
#define O_VFLTX 0xB8	/* vector convert to floating point */
#define O_VCMP  0xB9	/* vector compare */
#define O_VCPX  0xBA	/* vector compare */
#define O_VMINX 0xBB	/* vector minimum */
#define O_VFMF  0xBC	/* vector from mask floating point */
#define O_VFMIN 0xBD	/* vector floating minimum */
#define O_VMDA  0xBE	/* vector move data register to arithmetic register */
#define O_LVL   0xBF	/* load vector length */
#define O_WXS   0xC0	/*  */
#define O_WXM   0xC1	/*  */
#define O_AWXM  0xC2	/*  */
#define O_NWXM  0xC3	/*  */
#define O_VAND  0xC4	/* vector AND */
#define O_VOR   0xC5	/* vector OR */
#define O_VXOR  0xC6	/* vector XOR */
#define O_VEQV  0xC7	/* vector EQV */
#define O_VADD  0xC8	/* vector add */
#define O_VMPY  0xC9	/* vector multiply */
#define O_VADS  0xCA	/* vector add single */
#define O_VMPS  0xCB	/* vector multiply single */
#define O_VFAD  0xCC	/* vector floating point add */
#define O_VFMP  0xCD	/* vector floating point multiply */
#define O_VFIA  0xCE	/* vector floating iteration add */
#define O_VFIM  0xCF	/* vector floating iteration multiply */
#define O_RXS   0xD0	/*  */
#define O_RXM   0xD1	/*  */
#define O_ARXM  0xD2	/*  */
#define O_NRXM  0xD3	/*  */
#define O_VSLAX 0xD4	/* vector shift left arithmetic */
#define O_VSRAX 0xD5	/* vector shift right arithmetic */
#define O_VMRG  0xD6	/* vector merge */
#define O_DUMMYD7 0xD7	/*  */
#define O_VSUB  0xD8	/* vector subtract */
#define O_DUMMYD9 0xD9	/*  */
#define O_VSBS  0xDA	/* vector subtract single */
#define O_VMPYX 0xDB	/* vector multiply */
#define O_VFSB  0xDC	/* vector floating point subtract */
#define O_VFDV  0xDD	/* vector floating point divide */
#define O_VFIS  0xDE	/* vector floating iteration subtract */
#define O_DUMMYDF 0xDF	/*  */
#define O_RRS   0xE0	/* read remote access status */
#define O_RNA   0xE1	/* remote node access */
#define O_AWXMA 0xE2	/*  */
#define O_DUMMYE3 0xE3	/*  */
#define O_VSLD  0xE4	/* vector shift left double */
#define O_VSLL  0xE5	/* vector shift left logical */
#define O_VSLA  0xE6	/* vector shift left arithmetic */
#define O_DUMMYE7 0xE7	/*  */
#define O_VFIX  0xE8	/* vector convert to fixed point */
#define O_VFDB  0xE9	/* vector floating point double (each element is multiplied by 2.0) */
#define O_VSUM  0xEA	/* vector sum */
#define O_VSQR  0xEB	/* vector square */
#define O_VFSM  0xEC	/* vector floating sum */
#define O_VFSQ  0xED	/* vector floating square (^2) */
#define O_VIAM  0xEE	/* vector floating iteration add and multiply */
#define O_VIMA  0xEF	/* vector floating iteration multiply and add */
#define O_DUMMYF0 0xF0	/*  */
#define O_ARNA  0xF1	/* asynchronous remote node access */
#define O_ARXMA 0xF2	/*  */
#define O_DUMMYF3 0xF3	/*  */
#define O_VSRD  0xF4	/* vector shift right double */
#define O_VSRL  0xF5	/* vector shift right logical */
#define O_VSRA  0xF6	/* vector shift right arithmetic */
#define O_VBRV  0xF7	/* vector bit reverse */
#define O_VFLT  0xF8	/* vector convert to floating point */
#define O_VFHF  0xF9	/* vector floating half */
#define O_VCPS  0xFA	/* vector compare single */
#define O_VSQRX 0xFB	/* vector square (^2) */
#define O_VFCP  0xFC	/* vector floating compare */
#define O_VFDR  0xFD	/* vector floating divide reverse */
#define O_VISM  0xFE	/* vector floating iteration subtract and multiply */
#define O_VIMS  0xFF	/* vector floating iteration multiply and subtract */


/*
 * Instruction format
 */

/* 4 byte instructions */
#define F_RR       0x01
#define F_RW       0x02
#define F_RV       0x04
/* 8 byte instructions */
#define F_RX       0x10
#define F_CFX      0x20
#define F_RZ       0x40
#define F_RRX      0x80

#define FMASK_8BYTE 0xf0

/*
 * Operands format
 */
#define FM_XYZ   0x000  /* X, Y, Z */
#define FM_ASX   0x100  /* ASX */
#define FM_AS    0x200  /* AS */
#define FM_DXY_Z 0x300  /* X(Y), Z */
#define FM_DX_ZY 0x400  /* X, Z(Y) */
#define FM_DXZ_Y 0x500  /* (X, Z), Y */
#define FM_ZX    0x600  /* Z, X */
#define FM_XZY   0x700  /* X, Z, Y */

/* SX opcode arguments (operands) format */
#define SX_OA_FMT(x) 	((x) & 0x700)

/*
 * Field type.
 */

/* what type of data is in the field? */
#define TREG   0x0100 	/* field contains register */
#define TCONDF 0x0200 	/* field contains condition flags */
#define TDISPL 0x0400 	/* field contains displacement */
#define TLITER 0x0800 	/* field is a literal value */
#define TVDVC  0x1000	/* vector, vector data, and both */
#define TVDZC  0x2000 	/* vector data in z field check */
#define TSOFT  0x4000 	/* software controlled field */
#define TOPT   0x8000   /* if set, param is optional */

/* What type of register is in the field? */
/* in order to keep code clean, we implicitly add type bits here */
#define TREG_MASK (TREG|0x00F0)    /* mask for selecting register types */
#define TSREG     (TREG|0x0010) 	/* Scalar register */
#define TVREG     (TREG|0x0020) 	/* Vector register */
#define TVMREG    (TREG|0x0040) 	/* Vector Mask register */
#define TVDREG    (TREG|0x0080) 	/* Vector Data register */

/* What type of literal is in the field? */
/* in order keep code clean, we implicitly add type bits here */
#define TLIT_MASK (TLITER|0x000F)  /* mask for selecting literal types */
#define TLZERO    (TLITER|0x0001) 	/* literal value is zero */
#define TLIS63    (TLITER|0x0002) 	/* signed int between -64..63 */
#define TLM0M1    (TLITER|0x0004) 	/* 64bit (m)0, (m)1, m=0..63 */
#define TLI127    (TLITER|0x0008) 	/* int between 0..127 */

/*
 * Operand check flags
 */
#define FS_XFIX 0x1 /* fixed vreg in x */
#define FS_YFIX 0x2 /* fixed vreg in y */
#define FS_ZFIX 0x4 /* fixed vreg in z */

#define FS_VFIX_MASK 0x7

#define FS_SPOP	 0x08 /* single prec. operation ( mnemonic@ ) */
#define FS_MSKOP 0x10 /* masked operation ( mnemonic* ) */
#define FS_SELVU 0x20 /* select vector unit ( mnemonic% ) */
#define FS_SELEL 0x40 /* selection mode for element number, Cm=1 ( mnemonic? ) */
#define FS_ZEROH 0x80 /* high order 32 bits cleared to 0 ( mnemonic= ) */
                      /* Cm=1 for VLDL, VGTL, VLDLX (all SXes) 
		         or Cx=1 for LDL, DLDL (only SX5 upwards) */
#define FS_EXTOP 0x100  /* extended operation ( mnemonic! ) */
#define FS_VOVER 0x200  /* allow vector loads to overtake this operation ( mnemonic+ )*/
#define FS_BPYES 0x400  /* statical branch prediction: taken     ( mnemonic> )*/
#define FS_BPNO  0x800  /* statical branch prediction: not taken ( mnemonic< )*/

/*
 * Macros for instruction analysis
 */

/*
 * bitmask of certain number of bits (filled up from right to left)
 *
 */
#define CHARmask(bits,mode) (unsigned int)((mode) & (0xff >> (8 - (bits))))

/*
 * Bits in the SX documentation of assembler instructions are
 * described the "big endian" way, i.e. bit 0 is the leftmost one.
 */

/* check bit in a char */
#define CHARbit(bit, mode) ((mode) & (1 << (7 - (bit))))
/* set bit in a char */
#define sCHARbit(var, bit, val) (var = ((val)?(var | (1 << (7 - bit))):(var & ~(1 << (7 - bit)))))

/* check bit in a short */
#define SHRTbit(bit, mode) ((mode) & (1 << (15 - (bit))))
/* set bit in a short */
#define sSHRTbit(var, bit, val) (var = ((val)?(var | (1 << (15 - bit))):(var & ~(1 << (15 - bit)))))

/* check bit in an int */
#define INTbit(bit, mode) ((mode) & (1 << (31 - (bit))))
/* set bit in an int */
#define sINTbit(var, bit, val) (var = ((val)?(var | (1 << (31 - bit))):(var & ~(1 << (31 - bit)))))

/* fixed vector register index with the functional naming scheme */
#define VA0	1
#define VA1	2
#define VP0	3
#define VP1	4
#define VL0	5
#define VL1	6
#define VS0	7
#define VS1	8
#define VS0VS1	9
#define VA0VP1	10

/* conditional flags */
#define CONDF_INVALID ((unsigned char)-1)

#define CONDF_FALSE   ((unsigned char)0x00)

/* the following return false if == NAN */
#define CONDF_GT      ((unsigned char)0x01)
#define CONDF_LT      ((unsigned char)0x02)
#define CONDF_NEQ     ((unsigned char)0x03)
#define CONDF_EQ      ((unsigned char)0x04)
#define CONDF_GE      ((unsigned char)0x05)
#define CONDF_LE      ((unsigned char)0x06)

#define CONDF_NNAN    ((unsigned char)0x07)
#define CONDF_NAN     ((unsigned char)0x08)

/* the following return true if == NAN */
#define CONDF_GT_NAN  ((unsigned char)0x09)
#define CONDF_LT_NAN  ((unsigned char)0x0a)
#define CONDF_NEQ_NAN ((unsigned char)0x0b)
#define CONDF_EQ_NAN  ((unsigned char)0x0c)
#define CONDF_GE_NAN  ((unsigned char)0x0d)
#define CONDF_LE_NAN  ((unsigned char)0x0e)

#define CONDF_TRUE    ((unsigned char)0x0f)

#endif /* __OPCODES_SX_H__ */

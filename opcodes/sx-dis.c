/* Disassembler for NEC SX machines.

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
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, U\SA.

   Authors: Erich Focht <efocht@hpce.nec.com>
            jaKa Mocnik <jaka@xlab.si>
            Marko Novak <marko.novak@xlab.si>
            Matthias Hess
*/

#include <stdio.h>
#include "sysdep.h"
#include "dis-asm.h"
#include "opcode/sx.h"
#include <arpa/inet.h>


/* This file provides a disassembler function which uses
   the disassembler interface defined in dis-asm.h.   */

extern const struct sx_opcode sx_opcodes[];
extern const int sx_num_opcodes;

static int lendian = -1;

static int is_little_endian(void)
{
    long one = 1L;

    if (lendian == -1)
        lendian = (*((char *)(&one)));
    return lendian;
}

/* Print mnemonic suffix */
static void print_suffix_sx (disassemble_info *info, sx_instr *ins,
                             sx_opcode *op)
{
    unsigned short chk = op->flag;
    short fmt = op->format;
    char xfield = ins->opc.w1.x;
    unsigned char opfield = ins->opc.w1.op;
    int flag;

	if ((chk & FS_MSKOP) && CHARbit(0, xfield)) {   
    (*info->fprintf_func)(info->stream, "*");
  }
		
	if ((chk & FS_SELVU) && CHARbit(2, xfield))
		(*info->fprintf_func)(info->stream, "%%");

	if ((chk & FS_SELEL) && CHARbit(3, xfield))
		(*info->fprintf_func)(info->stream, "?");
 
	if (((chk & FS_ZEROH) && (fmt & F_RX) && CHARbit(0, xfield)) ||
    ((chk & FS_ZEROH) && CHARbit(3, xfield)))
		(*info->fprintf_func)(info->stream, "="); //and Cx bit check?

	if ((fmt & F_CFX) && CHARbit(2, xfield)) {
		if (CHARbit(3, xfield))
			(*info->fprintf_func)(info->stream, ">");
		else
			(*info->fprintf_func)(info->stream, "<");
	}

	if ((chk & FS_EXTOP) && CHARbit(0, xfield))
		(*info->fprintf_func)(info->stream, "!");

	if (chk & FS_SPOP) {
		flag = 0;

		if (fmt & F_RV) {
			if ((opfield == O_VFMAX || opfield == O_VFMIN)) {
				if (CHARbit(4,xfield))
					flag = 1;
			} else {
				if (CHARbit(3,xfield))
					flag = 1;
			}
		} else if ((fmt & F_CFX) || (fmt & F_RR)) {
			if (opfield == O_VFMF) {
				if (CHARbit(3,xfield))
					flag = 1;
			} else {
				if (CHARbit(0,xfield))
					flag = 1;
			}
		}
		if (flag)
			(*info->fprintf_func)(info->stream, "@");
	}
}


static void print_sx_reg(disassemble_info *info,
                         unsigned short flag, unsigned char field,
                         unsigned short dflag, int dfield)
{
  if ((flag & TREG_MASK) == TSREG || (flag & TREG_MASK) == (TSREG|TVREG))
    /* the (TSREG|TVREG) option is also handled here, because the TVREG option
       represents the fixed vector register which is never stored in the field 
       itself. */
		(*info->fprintf_func)(info->stream, "$s%d", CHARmask(7,field));
	else if ((flag & TREG_MASK) == TVREG)
		(*info->fprintf_func)(info->stream, "$v%d", CHARmask(3,field));
	else if ((flag & TREG_MASK) == TVMREG)
		(*info->fprintf_func)(info->stream, "$vm%d", CHARmask(4,field));
	else if (CHARbit(0, dflag)) {
		int vdreg;
        
		if (CHARbit(0, dfield)) {
			vdreg = INTbit(12, field) ? 0x80 : 0;
			vdreg = vdreg & CHARmask(7, field);
		}
        else {
			vdreg = ((CHARbit(4, field) << 4) + dfield);
        }
		(*info->fprintf_func)(info->stream, "$vd%d", vdreg);
	}
    else {
		(*info->fprintf_func)(info->stream, "$vd%d", CHARmask(7,field));
    }
}

static void print_sx_literal(disassemble_info *info,
                             unsigned short flag, unsigned char field)
{
    if ((flag & TLIT_MASK) == TLI127)
		(*info->fprintf_func)(info->stream, "%d", CHARmask(7,field));
	else if ((flag & TLIT_MASK) == TLIS63) {
		int val;
        
		if (CHARbit(1,field))
			val = (int)((unsigned int)field | 0xffffff80);
		else
			val = CHARmask(7,field);
		(*info->fprintf_func)(info->stream, "%d", val);
	}
    else if ((flag & TLIT_MASK) == TLM0M1)
		(*info->fprintf_func)(info->stream, "(%d)%d",
                              CHARmask(6,field),
                              CHARbit(1,field) ? 0 : 1);
	else
		(*info->fprintf_func)(info->stream, "%d", CHARmask(7,field));
}

/*
 * VD register for F_RRX
 */
static void print_sx_vd_dfield(disassemble_info *info, int dfield)
{
	unsigned char x, z;
	int vdreg = 0;
	
	x = (dfield & 0xff0000) >> 16;
	z = (dfield & 0xff);
    
	if (CHARbit(0, z))
		vdreg = vdreg & CHARmask(7, x);
	else
		vdreg = ((CHARbit(4, x) << 4) + z);
	(*info->fprintf_func)(info->stream, "$vd%d", vdreg);
}

static void
print_sx_tsoft_field(disassemble_info *info, unsigned char opcode, 
  unsigned char field)
{
  switch (opcode)
    {
    case O_VLDX:
    case O_VLDUX:
    case O_VLDLX:
      print_sx_literal(info, TLIS63, field);
      break;
    /*case :
      print_sx_literal(info, TLI127, field);
      break; */
    default:
      fprintf(stderr, "ERROR: unidentified print function for opcode %d\n.", 
        opcode);
      abort();
    }
}

static void print_sx_opc_field(disassemble_info *info, unsigned char opcode,
                               unsigned short flag, unsigned char field,
                               unsigned short dflag, int dfield)
{
    if((flag & TLITER) == TLITER && !CHARbit(0, field)) {
        print_sx_literal(info, flag, field);
    }
    else if ((flag & (TSREG|TSOFT)) == (TSREG|TSOFT) && !CHARbit(0, field)) {
      /* this one is for printing literals when we have TSREG|TSOFT-type 
         fields. */
      print_sx_tsoft_field(info, opcode, field);
    }
    else if(((flag & TREG) == TREG) ||
            ((flag & TVDVC) == TVDVC) ||
            ((flag & TVDZC) == TVDZC)) {
        print_sx_reg(info, flag, field, dflag, dfield);
    }
    else if((flag & TCONDF) == TCONDF) {
        (*info->fprintf_func)(info->stream, "%d", CHARmask(3, field));
    }
    else if((flag & TDISPL) == TDISPL) {
        if (dfield >= 0)
            (*info->fprintf_func)(info->stream, "0x%lx", dfield);
        else
            (*info->fprintf_func)(info->stream, "-0x%lx", abs(dfield));
    }
    else if((flag & TSOFT) == TSOFT) {
        (*info->fprintf_func)(info->stream, "%d", CHARmask(8,field));
    }
    else {
        fprintf(stderr, "ERROR: unhandled flag %04x for opcode %d\n.", flag, opcode);
        abort();
    }
}

/*
 * print fixed V registers
 */

static void print_sx_fixed_vreg(disassemble_info *info, unsigned short flag,
                                unsigned char field, unsigned char reg)
{
	if ((flag & FS_VFIX_MASK) && CHARbit(2,field))	/* Cf flag : 1 */
		(*info->fprintf_func)(info->stream, "%s", sx_fixed_vreg2[reg]);
	else
		(*info->fprintf_func)(info->stream, "%s", sx_fixed_vreg[reg]);
}


static void print_opcode_args(disassemble_info *info, sx_instr *ins,
                              sx_opcode *op)
{
	int comma = 0; // flag for comma
	int par = 0;   // flag for paranthesis
    
	switch (SX_OA_FMT(op->format)) {
	case FM_XYZ:  /* normal format X,Y,Z: this is the default if no other FM_* is
                     specified in the chk field of the opcode table */
		if (op->field.x) {
            print_sx_opc_field(info, op->opcode,
                               op->field.x, ins->opc.w1.x,
                               0, ins->d);
			comma = 1;
		}
		if (op->flag & FS_YFIX) {
			if (comma)
				(*info->fprintf_func)(info->stream, ",");
			print_sx_fixed_vreg(info, op->flag,
                                ins->opc.w1.x,
                                op->vreg.y);
			comma = 1;
		} else if (op->field.y) {
			if (comma)
				(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.y, ins->opc.w1.y,
                               0, ins->d);
			comma = 1;
		}
		if (op->flag & FS_ZFIX) {
			if (comma)
				(*info->fprintf_func)(info->stream, ",");
			print_sx_fixed_vreg(info, op->flag,
                                ins->opc.w1.x,
                                op->vreg.z);
		} else if (op->field.z) {
			if (comma)
				(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               op->field.d, ins->d);
		}
		break;
        
	case FM_DXZ_Y:  // (X,Z),Y
		if (!CHARbit(1, ins->opc.w1.x)) {
			if (op->format & F_RRX) {
				if (!CHARbit(0, ins->d))     /* check d field */
					par = 1;
			} else if (!CHARbit(0, ins->opc.w1.z))
				par = 1;
            
			if (par) {
				(*info->fprintf_func)(info->stream, "(");
				print_sx_opc_field(info, op->opcode,
                                   op->field.x, ins->opc.w1.x,
                                   0, ins->d);
				(*info->fprintf_func)(info->stream, ",");
				if (op->format & F_RRX)
					print_sx_vd_dfield(info, ins->d);
				else
					print_sx_opc_field(info, op->opcode,
                                       op->field.z, ins->opc.w1.z,
                                       op->field.d, ins->d);
				(*info->fprintf_func)(info->stream, ")");
			} else
				print_sx_opc_field(info, op->opcode,
                                   op->field.x, ins->opc.w1.x,
                                   0, ins->d);
		} else if (op->format & F_RRX) {
			if (!CHARbit(0, ins->d))	       /* check d field */
				print_sx_vd_dfield(info, ins->d);
			else if (!CHARbit(0, ins->opc.w1.z))
				print_sx_opc_field(info, op->opcode,
                                   op->field.z, ins->opc.w1.z,
                                   op->field.d, ins->d);
		} else {		/* only $vdz (F_RV or F_RR) */
			if (!CHARbit(0, ins->opc.w1.z))
				print_sx_opc_field(info, op->opcode,
                                   op->field.z, ins->opc.w1.z,
                                   op->field.d, ins->d);
		}
		if (op->flag & FS_XFIX) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_fixed_vreg(info, op->flag, ins->opc.w1.x,
                                op->vreg.x);
		}
		if (op->flag & FS_YFIX) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_fixed_vreg(info, op->flag, ins->opc.w1.x,
                                op->vreg.y);
		} else if (op->field.y) {
			(*info->fprintf_func)(info->stream, ",");
			if ((op->format & F_RV) &&
			    ins->opc.w1.op != O_VBRD &&
			    CHARbit(0, ins->opc.w1.y) &&
			    CHARmask(7, ins->opc.w1.y) == 0x7f)
				print_sx_fixed_vreg(info, op->flag,
                                    ins->opc.w1.x,
                                    op->vreg.y);
			else
				print_sx_opc_field(info, op->opcode,
                                   op->field.y, ins->opc.w1.y,
                                   0, ins->d);
		}
		if (op->flag & FS_ZFIX) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_fixed_vreg(info, op->flag, ins->opc.w1.x,
                                op->vreg.z);
		}
		if (op->format & F_RRX) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               op->field.d, ins->d);
		}
		break;
        
	case FM_DX_ZY: /* X, Z(Y) */
	case FM_DXY_Z: /* X(Y), Z */
	case FM_XZY:   /* X, Z, Y */
		if (op->field.x) {
			print_sx_opc_field(info, op->opcode,
                               op->field.x, ins->opc.w1.x,
                               0, ins->d);
			comma = 1;
		}
		if (comma && op->field.z &&
		    ((SX_OA_FMT(op->format) == FM_XZY) || (SX_OA_FMT(op->format) == FM_DX_ZY))) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               op->field.d, ins->d);
		}
		if (op->field.y) {
            if ((SX_OA_FMT(op->format) == FM_XZY) && comma)
                (*info->fprintf_func)(info->stream, ",");
            if ((SX_OA_FMT(op->format) == FM_DXY_Z) || (SX_OA_FMT(op->format) == FM_DX_ZY))
                (*info->fprintf_func)(info->stream, "(");
            print_sx_opc_field(info, op->opcode,
                               op->field.y, ins->opc.w1.y,
                               0, ins->d);
            comma = 1;
            if ((SX_OA_FMT(op->format) == FM_DXY_Z) || (SX_OA_FMT(op->format) == FM_DX_ZY))
                (*info->fprintf_func)(info->stream, ")");
		}
		if (op->field.z && (SX_OA_FMT(op->format) == FM_DXY_Z)) {
            if (comma)
                (*info->fprintf_func)(info->stream, ",");
            print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               op->field.d, ins->d);
		}
		break;
		
	case FM_ZX:	/* Z, X */
		if (op->field.z) {
			print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               op->field.d, ins->d);
			comma = 1;
		}
		if (op->field.x) {
			if (comma)
				(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.x, ins->opc.w1.x,
                               0, ins->d);
		}
		break;
		
	case FM_ASX: /* disp(index,base) : base=sreg, index=sreg or immed val, disp=abs val */
	case FM_AS:  /* disp(,base)   (AS is ASX without index specification) */
		if (op->field.x) {
			print_sx_opc_field(info, op->opcode,
                               op->field.x, ins->opc.w1.x,
                               0, ins->d);
			if (SX_OA_FMT(op->format) == FM_AS)
				(*info->fprintf_func)(info->stream, ",");
		}
		if (op->field.y && (SX_OA_FMT(op->format) == FM_AS)) {
			print_sx_opc_field(info, op->opcode,
                               op->field.y, ins->opc.w1.y,
                               0, ins->d);
		}
		if (op->field.d) {
			(*info->fprintf_func)(info->stream, ",");
            /* TODO: this printout if a biut strange to me ... */
			print_sx_opc_field(info, op->opcode,
                               op->field.d, ins->opc.w1.y,
                               0, ins->d);
		}
		(*info->fprintf_func)(info->stream, "(");
		if (op->field.y && (SX_OA_FMT(op->format) == FM_ASX)) {
			print_sx_opc_field(info, op->opcode,
                               op->field.y, ins->opc.w1.y,
                               0, ins->d);
		}
		if (op->field.z) {
			(*info->fprintf_func)(info->stream, ",");
			print_sx_opc_field(info, op->opcode,
                               op->field.z, ins->opc.w1.z,
                               0, ins->d);
		}
		(*info->fprintf_func)(info->stream, ")");
		break;
	}
    
}


/* Print a SX instruction.  */

int print_insn_sx (bfd_vma memaddr, disassemble_info *info)
{
	/* const struct sx_opcode *index; */
	bfd_byte buffer[4] ATTRIBUTE_UNUSED;
	sx_instr inst;
	sx_opcode *op;
	int iop;
	int status;
	int len = -1;
	int insn_format ATTRIBUTE_UNUSED;
    
	/* We can expect at least 32 bit instruction code */
	status = (*info->read_memory_func)(memaddr, (bfd_byte *)&inst, 4, info);
	if (status) {
		(*info->memory_error_func)(status, memaddr, info);
		return -1;
	}
	len = 4;
	(*info->fprintf_func)(info->stream, " : %08lx ",
		(is_little_endian()?htonl(inst.opc.w2):(unsigned int)inst.opc.w2));
    
	iop = sx_find_opc_idx(inst.opc.w1.op);
	if (iop < 0) {
		/* opcode not found: don't do a thing - it's just data
		   put in text segment, so we'll be quite happy with just
		   a memory dump */
		return len;
	}
	op = (sx_opcode *)&sx_opcodes[iop];
    
	if (op->format & FMASK_8BYTE) {
		status = (*info->read_memory_func)(memaddr+4,
                                           (bfd_byte *)&(inst.d),
                                           4,
                                           info);
		if (status) {
			(*info->memory_error_func)(status, memaddr+4, info);
			return -1;
		}
		len += 4;
		/* little endian handling */
		if (is_little_endian())
			inst.d = htonl(inst.d);
		(*info->fprintf_func)(info->stream, "%08lx\t", inst.d);
        
	}
    else {
		inst.d = 0;
		(*info->fprintf_func)(info->stream, "\t\t");
	}
    
	switch (op->format & 0xff) {
	case F_RR:
		switch (inst.opc.w1.op) {
		case O_LSR:
			op = (sx_opcode *)
				&sx_opcodes[iop +
                            (int)CHARbit(7, inst.opc.w1.z)];
			break;
            
		case O_SSR:
			op = (sx_opcode *)
				&sx_opcodes[iop +
                            (int)CHARmask(2, inst.opc.w1.z)];
			break;
            
		case O_LMIR:
		case O_SMIR:
            {
                int offset;
                
                if (CHARbit(0,inst.opc.w1.x))
                    offset = 0x19 +
                        CHARmask(5, inst.opc.w1.y);
                else
                    offset = CHARmask(5, inst.opc.w1.y);
                if (offset > 0x1a)
                    offset = 0x17; // generic "lmir" or "smir",
                // but actually this is wrong
                /* TODO: errrrm ... what is wrong, exactly?! could someone explain it? ;) */
                op = (sx_opcode *)&sx_opcodes[iop + offset];
                break;
            }
		}
		break;
        
	case F_CFX:
		switch (inst.opc.w1.op) {
		case O_BC:
		case O_BCF:
		case O_BCS:
			op = (sx_opcode *)
				&sx_opcodes[iop + (inst.opc.w1.x & 0xf)];
			break;
		}
		break;
	}
    
	/* print mnemonic */
	(*info->fprintf_func)(info->stream, "%s", op->name);
    
	/* print mnemonic suffix, if any */
	print_suffix_sx(info, &inst, op);
    
	(*info->fprintf_func)(info->stream, "\t");
    
	/* *** opcode arguments follow *** */
	print_opcode_args(info, &inst, op);
    
	(*info->fprintf_func)(info->stream, "\n");
    
	return len;
}

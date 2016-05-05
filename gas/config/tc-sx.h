/* Assembler for NEC SX machines.

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

#ifndef __TC_SX_H__
#define __TC_SX_H__

#define TC_SX

#include "opcode/sx.h"

#define TARGET_FORMAT "coff-sx64"
#define TARGET_ARCH   bfd_arch_sx
#define TARGET_NAME   "coff-sx64"

#define TARGET_BYTES_BIG_ENDIAN 1

/* NEC SX assembler code does not have a leading . at the beginning of
 * a pseudo instruction. */
#define NO_PSEUDO_DOT 1

/* we (probably) don't need broken word processing due to specific semantics
   of jumps on SX (depends on how gcc port will be done, actually). */
#define WORKING_DOT_WORD 1

#define md_number_to_chars number_to_chars_bigendian

#define md_convert_frag(b,s,f)          { as_fatal ("SX convert_frag\n"); }

/* We don't need to do anything special for undefined symbols.  */
#define md_undefined_symbol(s) 0

extern void sx_end_of_source PARAMS((void));
#define md_end()            sx_end_of_source ()

/* in SX assembler, the '$' character is a register prefix */
#define REGISTER_PREFIX '$'

/* some SX instructions contain an '=' character as a instruction suffix. */
#define TC_EQUAL_IN_INSN(C, NAME) 1

/* common SX sections */
#define SX_SEC_NAME_LCOMM   ".lcomm"
#define SX_SEC_NAME_SLCOMM  ".slcomm"
#define SX_SEC_NAME_COMMENT ".comment"

/* #define if we want to support weak symbols: SX/COFF format does not
   specify relevant storage classes, but it can't hurt if we introduce
   them and have GNU assembler and linker handle them: this allows us
   to support weak symbols (and aliases) in gcc */
#define SX_SUPPORT_WEAK_SYMBOLS

#define SX_BITS_PER_CHAR    8


#endif /* !__TC_SX_H__ */

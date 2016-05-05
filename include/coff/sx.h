/* COFF information for NEC SX machines.
   
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
#ifndef __COFF_SX_H__
#define __COFF_SX_H__

/* our target name */
#define COFF_SX_TARGET_BIG_NAME "coff-sx64"

#define SX_PACKED __attribute__((__packed__))

/* define the following if extended aux entry variants should be supported */
#undef SX_EXTENDED_AUX

/* our own debug functions */
#ifdef SX_ENABLE_DEBUG
  #define SX_DEBUG(f...) do { fprintf(stderr, "SX  " f); } while(0) 
#else
  #define SX_DEBUG(f...)
#endif 

#define BFD_IS_SX_COFF_TARGET(abfd) \
  (0 == strcmp(bfd_get_target(abfd), COFF_SX_TARGET_BIG_NAME))

#define DO_NOT_DEFINE_AOUTHDR
#define DO_NOT_DEFINE_SCNHDR
#define DO_NOT_DEFINE_FILHDR
#define DO_NOT_DEFINE_SYMENT
#define DO_NOT_DEFINE_AUXENT
#define DO_NOT_DEFINE_LINENO

#define NO_TVNDX

/* Some of these are required in 'coff/external.h' */

#define E_SYMNMLEN      16       /* # characters in a symbol name */
#define E_FILNMLEN      36       /* # characters in a file name */
/* FIXME: in fact, DIMNUM is 5 on SX, but we don't care at the moment */
#define E_DIMNUM        4        /* # array dimensions in auxiliary entry */

#include "coff/external.h"

/* file header magic numbers for different SX CPU models */
#define SX3MAGIC    02040
#define SX4MAGIC    02042
/* SX5 magic number is used on SX5 and SX6 models */
#define SX5MAGIC    02043
#define SX8MAGIC    02044
#define SX9MAGIC    02045

/* aout header magic numbers */
#define AOUT_MG_TEXTDATA    (0407) /* text not wr-protected or sharable;
                                      data contiguous with text */
#define AOUT_MG_LML32       (0410) /* data segment starts at the next segment
                                      following the text segment and the text
                                      segment is write-protected (for the large
                                      paging mode LM in 32G layout). */
#define AOUT_MG_LMS         (0411) /* data segment starts at the next segment
                                      following the text segment and the text
                                      segment is write-protected (for the small
                                      paging mode LM). */
#define AOUT_MG_LML512      (0420) /* data segment starts at the next segment
                                      following the text segment and the text
                                      segment is write-protected (for the large
                                      paging mode LM in 512G layout). */
#define AOUT_MG_LML32MT     (0510) /* data segment starts at the next segment
                                      following the text segment and the text
                                      segment is write-protected. Multitasking
                                      mode (for the large paging mode LM in 32G
                                      layout). */
#define AOUT_MG_LMLL512MT   (0520) /* data segment starts at the next segment
                                      following the text segment and the text
                                      segment is write-protected. Multitasking
                                      mode (for the large paging mode LM in
                                      512G layout). */


/* object and executable file data structures */

struct SX_PACKED external_filehdr {
	char f_magic[2];
	char f_flags3;
	char pad;
	char f_nscns[4];
	char f_timdat[4];
	char unused[4];
	char f_symptr[8];
	char f_nsyms[8];
	char f_opthdr[4];
	char f_flags[2];
	char f_flttype;
	char f_flags2;
};
#define FILHDR  struct external_filehdr
#define FILHSZ  40

/* 'Optional' aout header. This header is actually mandatory
  so the name is a bit misleading.
*/
struct SX_PACKED external_aouthdr {
	char magic[2];
	char vstamp[2];
	char unused[4];
	char tsize[8];
	char dsize[8];
	char bsize[8];
	char entry[8];
	char text_start[8];
	char data_start[8];
	char dummy5[4];
	char unused2[4];
	char ssize[8];
	char dummy6[8];
	char dummy2[8];
	char dummy3[8];
	char dummy4[8];
};
#define AOUTHDR struct external_aouthdr
#define AOUTSZ 104

/* section header */
struct SX_PACKED external_scnhdr {
        char s_name[8];
        char s_paddr[8];
        char s_vaddr[8];
        char s_size[8];
        char s_scnptr[8];
        char s_relptr[8];
        char s_lnnoptr[8];
        char s_nreloc[4];
        char s_nlnno[4];
        char s_flags[4];
        char s_dummy;
        char reserved[3];
};
#define SCNHDR struct external_scnhdr
#define SCNHSZ 72

#define _TEXT		".text"
#define _DATA		".data"
#define _BSS		".bss"
#define _COMMENT	".comment"
#define _LIB		".lib"

/* relocation entry */
struct SX_PACKED external_reloc {
	char r_vaddr[8];
	char r_symndx[4];
	char r_type[2];
	char reserved[2];
};
#define RELOC struct external_reloc
#define RELSZ 16

/* line numbers don't seem to need external representation
   as the internal one is equivalent. */

/* symbol entries */

/* language flags for misc[0] field */
#define L_UNKNOWN 0
#define L_AS      1
#define L_C       2
#define L_F77     3
#define L_PC      4
#define L_C64     5
#define L_F90     6

struct SX_PACKED external_syment 
{
  union
  {
    char e_name[E_SYMNMLEN];

    struct
    {
      char e_zeroes[8];
      char e_offset[8];
    } e;
    char *_e_nptr[2];
  } e;

  char e_value[8];
  char e_scnum[4];
  char e_type[4];
  char e_sclass[1];
  char e_numaux[1];
  char _e_misc[2];
};

#define SYMENT  struct external_syment
#define SYMESZ  36

/* the SX COFF has type field 8 bits long */
#define N_BTMASK        0xff 
#define N_BTSHFT        8
/* the SX COFF has 12 2-bit fields for derived types */
#define N_TMASK         0x00000300 
#define N_TSHIFT        2
/* FIXME: currently, we only care about the first derivative (no, it's nothing
   mathematical;). that is: we will only consider the first 2 bits after the
   first byte */

union SX_PACKED external_auxent {
  struct {
    char x_tagndx[4];
    char x_type[4];
    union {
      struct {
        char x_lnno[4];
        char x_size[4];
      } x_lnsz;
      char x_fsize[8];
    } x_misc;
    
    union {
      struct {
        char x_lnnoptr[8];
        char x_endndx[4];
        char x_stksz[4];
        char x_tvndx[4];
      } x_fcn;
      
      struct {
        char x_dimen[5][4];
      } x_ary;

      struct {
        char x_span[4];
        char x_csize[4];
      } x_fot1;

      struct {
        char x_dummy1[8];
        char x_dummy2[8];
        char x_attr[2];
        char x_dummy3[2];
      } x_f90attr;

    } x_fcnary;
    
  } x_sym;
  
  union {
    char x_fname[E_FILNMLEN];
    struct {
      char x_zeroes[4];
      char x_offset[4];
    } x_n;
  } x_file;

  struct {
    char x_scnlen[8];
    char x_nreloc[4];
    char x_nlinno[4];
  } x_scn;
  
  struct {
    char x_tvfill[4];
    char x_tvlen[2];
    char x_tvran[4];
  } x_tv;

  /* the following aux stuff is currently not supported by internal
     representation, so there is no point in burdening the compiler
     with it - hence #if-ed out.
     perhaps we choose to support it someday ... */
#ifdef SX_EXTENDED_AUX
  struct {
    struct {
      char x_stype1;
      char x_stype2;
      char x_stype3;
      char x_dimen_low[4];
      char x_dimen_upp[4];
      char x_dimen_mult[4];
    } x_odd;

    struct {
      char x_stype1;
      char x_stype2;
      char x_stype3;
      char x_dimen_low[4];
      char x_dimen_upp[4];
      char x_dimen_mult[4];
    } x_even;
  } x_fot2;

  /* 2GB extension */
  struct {
    __LONG          x_tagndx;       /* end of struct, */
                                    /* function, */
                                    /* struct union enum block */
    __LONG          x_endndx;       /* function */
    union {
      __LLONG         x_size;         /* tag, */
                                      /* end of struct, */
                                      /* struct union enum block */
      __LLONG         x_fsize;        /* function */
    } x_ex_misc;
    union {
      struct {
	__LLONG         x_lnnoptr;      /* function */
	__LLONG         x_stksz;        /* function */
	unsigned __INT  x_tvndx;        /* function */
      } x_ex_fcn;
      struct {
	__LLONG         x_dummy1;
	__LONG          x_endndx;       /* tag */
      } x_ex_tag;
    } x_ex_fcnary;
  } x_ex_sym;

  struct {
    __LONG          x_tagndx;
    unsigned char   x_type;
    unsigned char   dummy1;
    unsigned char   dummy2;
    unsigned char   dummy3;
    __LLONG         x_size;
    __LLONG         x_dimen1;
    __LLONG         x_dimen2;
    unsigned __INT  x_lnno;
  } x_ex_c1;

  struct {
    __LLONG         x_dimen3;
    __LLONG         x_dimen4;
    __LLONG         x_dimen5;
  } x_ex_c2;

  struct {
    __LONG          x_tagndx;
    unsigned char   x_type;
    unsigned char   x_stype1;
    unsigned char   x_stype2;
    unsigned char   x_stype3;
    unsigned __INT  x_lnno;
    unsigned __INT  dummy1;
    __INT   x_span;
    unsigned __INT  x_csize;
    __LLONG         x_size;
    unsigned short  x_attr;
  } x_ex_fot1;
  
  struct {
    unsigned char   x_stype1;
    unsigned char   x_stype2;
    unsigned char   x_stype3;
    unsigned char   dummy1;
    unsigned __INT  dummy2;
    __LLONG         x_dimen_low;
    __LLONG         x_dimen_upp;
    __LLONG         x_dimen_mult;
  } x_ex_fot2;
#endif /* SX_EXTENDED_AUX */

};

#define AUXENT union external_auxent
#define AUXESZ 36

#define L_LNNO_SIZE 4

struct SX_PACKED external_lineno
{
  union
  {
    char l_symndx[8];	/* function name symbol index, iff l_lnno == 0*/
    char l_paddr[8];	/* (physical) address of line number	*/
  } l_addr;

  char l_lnno[L_LNNO_SIZE];	/* line number		*/
};
#define LINENO struct external_lineno
#define LINESZ (8 + L_LNNO_SIZE)

#endif /* !__COFF_SX_H__ */

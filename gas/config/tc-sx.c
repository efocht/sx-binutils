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

#include <stdarg.h>

#include "as.h"
#include "safe-ctype.h"
#include "subsegs.h"
#include "dwarf2dbg.h" 

#include "tc-sx.h"
#include "opcode/sx.h"
#include "struc-symbol.h"

/* pseudo-op fwd declarations */
static void sx_cons (int);
static void sx_repeat_cons (int);
static void sx_str (int);
static void sx_repeat_str (int);
static void sx_float (int);
static void sx_repeat_float (int);
static void sx_address (int);
static void sx_using (int);
static void sx_drop (int);
static void sx_even (int);
static void sx_lcomm (int);
static void sx_lglobal (int);
static void sx_lstat (int);
static void sx_taskcomm (int);
static void sx_bss (int);
static void sx_ident (int);
#ifdef SX_SUPPORT_WEAK_SYMBOLS
static void sx_weak (int);
static void sx_wcomm (int);
#endif /* SX_SUPPORT_WEAK_SYMBOLS */

static void sx_pseudo_unimpl (int);

const char comment_chars[] = "#";

const char line_comment_chars[] = "#";

const char line_separator_chars[] = ";";

const char EXP_CHARS[] = "eE";

const char FLT_CHARS[] = "dDfFqQ";

#define OPTION_SX_OPTION  (OPTION_MD_BASE + 0)

const char *md_shortopts = "h:";
struct option md_longopts[] = {
  { "sx-option", required_argument, NULL, OPTION_SX_OPTION},
};
size_t md_longopts_size = sizeof(md_longopts);

/* machine type (i.e. flavour) SX[4|5|6|8]; default is SX5/6 */
static int sx_flavour = bfd_mach_sx5;

/* assembler command line parameters BEGIN */

/* this flag determines the "Floating-Point Data Type of File Header" 
   (i.e. the "f_flttype" field in the SX COFF header)
   In the 5-3 table of the SX "Programming Language Support Reference Manual",
   its values are defined, as follows:
   * FT_NON: Does not use floating-point data.
   * FT_FL0: Uses float0 mode floating-point data (i.e. the IEEE floating-point
             data format).
   * FT_FL1: Uses float1 mode floating-point data (i.e. the IBM floating-point
             data format).
   * FT_FL2: Uses float2 mode floating-point data (i.e. the CRAY floating-point
             data format).
   * FT_MIX: Uses float0/float1/float2 mode floating-point data format. */
static unsigned char sx_float_mode = FT_FL0;




/* determines if the current object file is in LP64 mode (
   sizeof(short) == 16 bits; 
   sizeof(int) == 32 bits;
   sizeof(long) == 64 bits;
   sizeof(long long) == 64 bits;
   sizeof(pointer) == 64 bits;):
   1 = LP64 mode object file.
   0 = undefined 
   
   For further details, see the "Chapter 5: Common Object File Format (COFF)" 
   of the SX "Programming Language Support Reference Manual" */
static bfd_boolean sx_f2_lp64 = FALSE;

/* FIXME: currently we always assume that we are compiling the C language.
   Perhaps there is some information in the "gcc" that could tell "gas"
   which language is currently being compiled. */
static bfd_boolean sx_f2_c_lang = FALSE;

/* the combination of the "sx_f2_bit6_used" and "" variables determines the 
   mode of the object file:
   1 = object file is in int64 mode (the size of integers is 64 bits). 
   0 = object file is in int32 mode (the size of integers is 32 bits). */
static bfd_boolean sx_f2_bit6_used = FALSE;
static bfd_boolean sx_f2_int64 = FALSE;

/* determines if vector instructions are used */
static bfd_boolean sx_f2_vector_instr = FALSE;



/* determines if the "not depending on the size of size_t type" bit is 
   used. */
static bfd_boolean sx_f3_size_tmix = FALSE; 

/* determines if the "the size of size_t type is 64 bits" bit is used. */
static bfd_boolean sx_f3_size_t64 = FALSE;

/* determines the length of vector register:
 * 0 = vector register length is 256 bits
 * 1 = vector register length is 512 bits */
static bfd_boolean sx_f3_vl512_only = FALSE;

/* determines if the "vector register length is 512 or 256" bit is used. */
static bfd_boolean sx_f3_max_vl512_ready = FALSE;

/* determines if the "float0 P64 mode object" bit is used. */
static bfd_boolean sx_f3_p64 = FALSE;

/* bits 5 and 6 are undefined for object files (should be zero). */

/* determines if the "this is multitasking object" bit is used. */
static bfd_boolean sx_f3_multi_tasking = FALSE;

/* assembler command line parameters END */

#ifdef SX_OBSOLETE_OPTIONS
#define SX_GAS_OPTIONS                          \
  "Supported options:\n"                        \
  "\tsx4, sx5, sx6, sx8, sx9\n"                 \
  "\tfloat0, float1, float2\n"                  \
  "\tsize_t32, size_t64\n"                      \
  "\tmaxvl512, vl512\n"
#define SX_GAS_OPTIONS_LONG                                             \
  "\tsx4, sx5, sx6, sx8, sx9: choose target machine type.\n"            \
  "\tfloat0: use float0 mode floating-point data (i.e. the IEEE "       \
  "floating-point data format).\n"                                      \
  "\tfloat1: use float1 mode floating-point data (i.e. the IBM "        \
  "floating-point data format).\n"                                      \
  "\tfloat2: use float2 mode floating-point data (i.e. the CRAY "       \
  "floating-point data format).\n"                                      \
  "\tsize_t32, size_t64: set the size of the 'size_t' type to be \n"    \
  "\t\t32 or 64 bits.\n"                                                \
  "\tmaxvl512: maximum vector register length is 512 elements\n"        \
  "\tvl512: vector register length is 512 elements\n"
#else
#define SX_GAS_OPTIONS                          \
  "Supported options:\n"                        \
  "\tsx6, sx8, sx9\n"                           \
  "\tsize_t32, size_t64\n"                      \
  "\tmaxvl512, vl512\n"
#define SX_GAS_OPTIONS_LONG                                             \
  "\tsx6, sx8, sx9: choose target machine type.\n"                      \
  "\tsize_t32, size_t64: set the size of the 'size_t' type to be \n"    \
  "\t\t32 or 64 bits.\n"                                                \
  "\tmaxvl512: maximum vector register length is 512 elements\n"        \
  "\tvl512: vector register length is 512 elements\n"
#endif /* SX_OBSOLETE_OPTIONS */

int
md_parse_option(int c, char *arg)
{
  switch(c)
    {
    case 'h':
    case OPTION_SX_OPTION:
      /* choose flavour. */
#ifdef SX_OBSOLETE_OPTIONS
      if(0 == strcmp(arg, "sx4"))
        sx_flavour = bfd_mach_sx4;
      else if(0 == strcmp(arg, "sx5"))
        sx_flavour = bfd_mach_sx5;
      else
#endif /* SX_OBSOLETE_OPTIONS */
      if(0 == strcmp(arg, "sx6"))
        sx_flavour = bfd_mach_sx5;
      else if(0 == strcmp(arg, "sx8"))
        sx_flavour = bfd_mach_sx8;
      else if(0 == strcmp(arg, "sx9"))
        sx_flavour = bfd_mach_sx9;
#ifdef SX_OBSOLETE_OPTIONS
      /* choose floating point data format. */
      else if (0 == strcmp(arg, "float0"))
        sx_float_mode = FT_FL0;
      else if (0 == strcmp(arg, "float1"))
        sx_float_mode = FT_FL1;
      else if (0 == strcmp(arg, "float2"))
        sx_float_mode = FT_FL2;
#endif /* SX_OBSOLETE_OPTIONS */
      /* choose the size of "size_t" type. */
      else if (0 == strcmp(arg, "size_t32"))
        sx_f3_size_t64 = FALSE;
      else if (0 == strcmp(arg, "size_t64"))
        sx_f3_size_t64 = TRUE;
      /* choose vector register length to be 512 or less. */
      else if (0 == strcmp(arg, "maxvl512"))
        sx_f3_max_vl512_ready = TRUE;
      /* choose vector register length to be exactly 512. */
      else if (0 == strcmp(arg, "vl512"))
        sx_f3_vl512_only = TRUE;
        
      /* undefined option. */ 
      else
        {
          as_fatal("Invalid SX option: %s. "
                   SX_GAS_OPTIONS,
                   arg);
          return -1;
        }
      break;
      
    default:
      return 0;
    }
  return 1;
}


void
md_show_usage(FILE *stream)
{
  fprintf(stream,
          "  -h --sx-option=<sx-opt>  "
          "Provide SX assembler option. Supported options:\n"
          SX_GAS_OPTIONS_LONG
          );
}

/* a hash of mnemonics for O(1) lookups (constructed in md_begin()) */
static struct hash_control *op_hash = NULL;

const pseudo_typeS md_pseudo_table[] =
{
  /* section-related pseudo-ops */
  /* section: default impl */
  /* text: default impl */
  /* data: default impl */
  { "lcomm",  sx_lcomm, 0 },
  { "slcomm", sx_lcomm, 1 },

  /* implicit base addressing via using */
  { "using",  sx_using, 0 },
  { "drop",   sx_drop, 0 },

  /* alignment and other IC-related stuff */
  { "even",   sx_even, 0 },
  /* org: default impl */
  { "align",  s_align_bytes, 0 },    /* default impl aligns on a
					(2^param)-byte boundary */
  /* space: default impl */

  /* constants */
  { "byte",   sx_cons, 1 },
  { "short",  sx_cons, 2 },
  { "long",   sx_cons, 4 },
  { "llong",  sx_cons, 8 },
  { "str",    sx_str, 0 },
  { "strz",   sx_str, 1 },
  { "rbyte",  sx_repeat_cons, 1 },
  { "rshort", sx_repeat_cons, 2 },
  { "rlong",  sx_repeat_cons, 4 },
  { "rllong", sx_repeat_cons, 8 },
  { "rstr",   sx_repeat_str, 0 },
  { "rstrz",  sx_repeat_str, 1 },
  { "adcn",   sx_address, 8 },

  /* symbol classes */
  /* global: default impl */
  /* comm: default impl */
  { "bss", sx_bss, 0 },

  /* set: default impl */

  /* implemented and tested up to this point; missing functionality follows */

  { "version", sx_pseudo_unimpl, 0 },
  { "ident", sx_ident, 0 },

  /* symbol classes */
  { "lglobal", sx_lglobal, 0 },
  { "slglobal", sx_lglobal, 1 },
  { "taskcomm", sx_taskcomm, 0 },
  { "staskcomm", sx_taskcomm, 1 },
  { "lstat", sx_lstat, 0 },
  { "slstat", sx_lstat, 1 },

  /* float constants */
  { "float",  sx_float, 4 },
  { "double", sx_float, 8 },
  { "quadr",  sx_float, 16 },
  { "rfloat", sx_repeat_float, 4 },
  { "rdouble",sx_repeat_float, 8 },
  { "rquadr", sx_repeat_float, 16 },

#ifdef SX_SUPPORT_WEAK_SYMBOLS
  /* weak symbol related pseudo-ops are not supported by SX assembler
     although listed in reference booklet */
  { "weak", sx_weak, 0 },
  { "wcomm", sx_wcomm, 0 },
#endif /* SX_SUPPORT_WEAK_SYMBOLS */

  {0,0,0}
};

/****
 * Structures specific to SX gas
 ****/

/**** operands ****/

/* Mask type: immediate value of the form (m)0 or (m)1  (general form: (m)i ) */
typedef struct sx_imm_mask {
  int m;  /* m=0..63 */
  int i;  /* i=0 or i=1 */
} sx_imm_mask;

typedef struct {
  int type;        /* TSREG, TVREG, TVMREG, TVDREG.
                      A value of -1 means not defined. */
  int num;         /* Number of register */
  int constraints;
} sx_reg_entry;

enum sx_idx_type {
  SX_IDX_NONE,
  SX_IDX_REG,
  SX_IDX_IMM,
};

typedef struct sx_idx {
  enum sx_idx_type type;
  union {
    sx_reg_entry scalar_reg;
    int imm;
  } data;
} sx_idx;

typedef struct {
  expressionS displ;      /* May contain label */
  sx_idx index;           /* May be scalar reg or immediate value */
  sx_reg_entry base;      /* Base register */
} sx_asx_entry;

enum sx_immediate_type {
  IMM_TYPE_I,
  IMM_TYPE_N  
};

enum sx_vector_result_reg_type {
  VRES_VREG,
  VRES_VDREG,
  VRES_BOTH
};

typedef struct {
  enum sx_vector_result_reg_type type;
  sx_reg_entry vreg;
  sx_reg_entry vdreg;
} sx_vres_entry;

/*
 * The operand structure must be able to hold:
 * - register information (type , register number)
 * - address syllables in ASX or AS format: displ(index, base)
 * - immediate values (literals)
 *   - (m)1  or  (m)0   (with m=0..63)
 *   - signed 7 bit numbers  -64 .. +63
 *   - unsigned 7 bit number 0 .. 127
 *
 * ... anything else?
 */

#define OP_IMM     1
#define OP_MASK    2
#define OP_REG     3
#define OP_ASX     4
#define OP_VRES    5

struct sx_op {
  int type;                  /* operand type: check out OP_* macros above */
  union {
    sx_reg_entry reg;        /* register */
    struct sx_imm_mask mask; /* (m)0, (m)1 mask */
    char imm;                /* immediate value */
    sx_asx_entry asx;        /* ASX, AS type */
    sx_vres_entry vec_res_reg; /* vector result (i.e. 
                                        [$vx | $vdz | ($vx,$vdz)]) register */
  } op;                      /* operand union */
};

enum sx_operand_type {
  SX_OP_NONE,
  SX_OP_X,
  SX_OP_Y,
  SX_OP_Z
};

/* maximal number of operands to check when parsing RR-type instructions */
#define MAX_NUM_OPERANDS 3

/**** parsed instruction ****/

struct sx_insn {
  int flags;
  sx_opcode *op; /* pointer to sx_opcode element from the big opcode table */
  struct sx_op ops[4];      /* SX instructions have at most 4 operands */
  int opcount;
};

static struct sx_insn inst;

/**** code generation ****/

/* The generated instruction, ready to be written out to file.
   The length (1 or two 32 bit words) is returned by the code generator
   when writing into this structure. */
static sx_instr code;
/* pointer to start of next instruction ... */
static char *current_ptr;


/**** "using" stuff ****/

static int          using_set;
static sx_reg_entry using_base_reg;
static expressionS  using_base_val;


/**** mnemonic suffixes (flags) ****/

#define SX_ISSPECIAL(x) ((x)=='*'||(x)=='%'||(x)=='?'||(x)=='='||(x)=='<'||\
                         (x)=='>'||(x)=='!'||(x)=='@'||(x)=='+')

/* Translate special character to flag representation */
#define SX_SPECIAL_FLAG(x) \
  (((x) == '*') ? FS_MSKOP : \
  (((x) == '%') ? FS_SELVU : \
  (((x) == '?') ? FS_SELEL : \
  (((x) == '=') ? FS_ZEROH : \
  (((x) == '<') ? FS_BPNO  : \
  (((x) == '>') ? FS_BPYES : \
  (((x) == '!') ? FS_EXTOP : \
  (((x) == '@') ? FS_SPOP  : \
  (((x) == '+') ? FS_VOVER : 0)))))))))


/**** fix-ups ****/

/* values for fx_addnumber member */

/* this is a displacement fix-up: consider it done, regardless of symbols
   (can be done according to SX assembler ASX semantics) */
#define FIX_DISPLACEMENT     ((valueT)42)


/**** non-standard sections ****/

struct sx_section_info
{
  char *name;
  segT section;
  flagword flags;
};

static struct sx_section_info sx_sections[] =
  {
#define SX_SEC_IDX_LCOMM  0
    { SX_SEC_NAME_LCOMM, NULL,
      SEC_ALLOC | SEC_LOAD | SEC_DATA | SEC_HAS_CONTENTS },
#define SX_SEC_IDX_SLCOMM 1
    { SX_SEC_NAME_SLCOMM, NULL,
      SEC_ALLOC | SEC_LOAD | SEC_DATA | SEC_HAS_CONTENTS },
#define SX_SEC_IDX_COMMENT 2
    { SX_SEC_NAME_COMMENT, NULL,
      SEC_HAS_CONTENTS },
  };
  
/* the SX supports single (2*2 bytes), double (4*2 bytes) and quadruple 
   (8*2 bytes) precision floating point numbers. */
#define MAX_LITTLENUMS 8

/*****************************************************************************/
/* Error detail reporting stuff                                              */
/*****************************************************************************/


#define ERR_MSG_MAX_LEN        256
static inline void 
sx_error(const char *fmt, ...)
{
  char errmsg[ERR_MSG_MAX_LEN + 1];

  va_list argp;

  /* first print an error message */
  va_start(argp, fmt);
  vsnprintf(errmsg, ERR_MSG_MAX_LEN, fmt, argp);
  as_bad(errmsg);
  va_end(argp);  
}


/*****************************************************************************/
/* Utility functions                                                         */
/*****************************************************************************/

/* if this gets executed, it's a *BUG* */
#define BUG(f ...)           as_fatal("BUG: " f)

#if 0
/* debugging print-out */
#define GAS_DBG_PRINT(f ...) fprintf(stderr, "### " f)
#define PSYM(s)              psym(s)

static void
psym(symbolS *sym)
{
  if(NULL == sym)
    {
      GAS_DBG_PRINT("NULL\n");
    }
  else
    {
      GAS_DBG_PRINT("Using symbol: %s\n", S_GET_NAME(sym));
      GAS_DBG_PRINT("Value: %llx\n", (unsigned long long) S_GET_VALUE(sym));
      GAS_DBG_PRINT("Is fwd: %d\n", S_IS_FORWARD_REF(sym));
    }
}
#else
#define GAS_DBG_PRINT(f ...) do { } while(0)
#define PSYM(s)              do { } while(0)
#endif

/* md_assemble() always leaves the strings it's passed unaltered.  To
   effect this we maintain a stack of saved characters that we've smashed
   with '\0's (indicating end of strings for various sub-fields of the
   assembler instruction).  */
/* TODO: do we really need this??? marko? */
static char save_stack[32];
static char *save_stack_p;
#define END_STRING_AND_SAVE(s) \
  do { *save_stack_p++ = *(s); *(s) = '\0'; } while (0)
#define RESTORE_END_STRING(s) \
  do { *(s) = *--save_stack_p; } while (0)


/* this macro defines the char that marks the end of instruction */
#define END_OF_INSN '\0'


inline static char *
sx_skip_whitespace(char *op)
{
  /* if the current char is space, then skip it 
   * NOTE: in gas we only need a single space skip, because the assembler 
   * instructions are already normalized (i.e. if there are multiple 
   * whitespace present, they are truncated). */
  if (ISSPACE(*op))
    ++op;
  
  return op;
}



/* extracts SX name (for a definition of name, look in SX "Assembly Language 
   Reference Manual", section 2.1 "Names" */
static char *
sx_extract_name(char *op)
{  
  /* names must be prefixed with an underscore (_), a period (.), or a letter 
   * (a-z, A-Z). These can be followed by alphanumeric characters.*/
  if (*op != '_' && *op != '.' && *op != REGISTER_PREFIX && !ISALPHA(*op)) {
    return NULL;
  }
  op++; /* move one character forward */
  
  /* The prefixes can be followed by alphanumeric characters. */
  while (ISALNUM(*op))
    {
      op++;
    }
  
  return op;
}

/* extracts hexadecimal number of arbitrary length. */
static char *
sx_extract_hex_num(char *op)
{
  /* first check if the string begins with "0x" */
  if (strstr(op, "0x") != op && strstr(op, "0X") != op)
    {
      return NULL;
    }
  /* skip the "0x" prefix. */
  op += 2;
  
  while (ISDIGIT(*op) || TOLOWER(*op) == 'a' || TOLOWER(*op) == 'b' ||
    TOLOWER(*op) == 'c' || TOLOWER(*op) == 'd' || TOLOWER(*op) == 'e' ||
    TOLOWER(*op) == 'f')
    {
      op++;
    }
    
  return op;
}

/* checks if a given string is a number */
inline static bfd_boolean
is_str_number(char *str)
{
  unsigned int i;
  for (i = 0; i < strlen(str); i++)
    {
      if (!ISDIGIT(str[i]))
        return FALSE;
    }
    
  return TRUE;
}


/* converts a number to its corresponding power-of-two;
   returns -1 if not a power of two */
inline static int
val_to_pow2(int val)
{
  int rv = 0;

  if(0 == val)
    {
      /* this is a bit "unmathlike" ;), but gcc produces
	 code with align param set to zero ... */
      return 1;
    }

  while((val & 1) == 0) {
    val >>= 1;
    rv++;
  }
  if(val != 1)
    return -1;
  return rv;
}


/* align to a byte boundary */
static int
sx_align_bytes(int nbytes)
{
  int p2 = val_to_pow2(nbytes);
  if(p2 < 0)
    return -1;
  if (p2 > 0 && !need_pass_2)
    {
      if (subseg_text_p (now_seg))
        frag_align_code (p2, 0);
      else
        frag_align (p2, 0, 0);
    }
  return 0;
}

/* set current section to one of SX common sections; create section
   stuff if necessary. */
static void
sx_set_section(int idx) 
{
  int sub = 0;

  if(NULL != sx_sections[idx].section)
    {
      subseg_set(sx_sections[idx].section, sub);
    }
  else
    {
      sx_sections[idx].section = subseg_new(sx_sections[idx].name, sub);
      bfd_set_section_flags (stdoutput, sx_sections[idx].section,
                             sx_sections[idx].flags);
      if((sx_sections[idx].flags & SEC_LOAD) == 0)
        {
          seg_info(sx_sections[idx].section)->bss = 1;
        }
    }
}


/*****************************************************************************/
/* Parsing: helper functions                                                 */
/*****************************************************************************/

inline static bfd_boolean
sx_is_register_even(sx_reg_entry *reg)
{
  return (reg->num % 2) == 0;
}

/*
  finding condf for a given opcode struct

  relies on an assumption that b*, b*s and b*f instructions are in the table
  so that:
  - are sequential (one sequential batch for each of b*, b*s and b*f)
  - each batch is sorted according to increasing value of condf

  could be optimized by caching the first instruction for each type [|s|f]
  instead of looking it up each time ...
*/
static unsigned char
sx_get_condf(sx_opcode *op) {

  int i;
  
  if(op->opcode != O_BC && op->opcode != O_BCS && op->opcode != O_BCF) {
    /* this is not an opcode that has conditional flags! */
    return CONDF_INVALID;
  }

  /* find first instruction with this opcode value (the one with condf == 0) */
  for(i = 0; i <= sx_num_opcodes; i++) {
    if(sx_opcodes[i].opcode == op->opcode) {
      break;
    }
  }
  if(i > sx_num_opcodes) {
    return CONDF_INVALID;
  }
  return (unsigned char)
    (((char *)(op) - (char *)(&sx_opcodes[i])) / sizeof(sx_opcode));
}

/***
 * utility functions for parsing various elements from the assembly source
 */

static char *
sx_parse_comma(char *op)
{
  /* Remove trailing spaces */
  op = sx_skip_whitespace(op);

  /* check if the current char is comma */
  if (*op != ',')  
    {
      return NULL;
    }
  
  /* if comma was indeed present, skip it */
  op++;
  return op;
}

/* parses SX expressions */
static char *
sx_parse_expression(char *op, expressionS *exp, enum expr_mode exp_type)
{
  char *ilp_save;
  
  /* first store the input line pointer because it is marks the beginning 
   * of the next assembly instruction */
  ilp_save = input_line_pointer;

  /* parse the displacement expression */
  input_line_pointer = op;
  
  switch (exp_type)
    {
    case expr_evaluate:
      expression_and_evaluate(exp);
      break;
    case expr_normal:
      expression(exp);
      break;
    case expr_defer:
      deferred_expression(exp);
      break;
    }
  
  op = input_line_pointer;
  
  /* restore the input line pointer */
  input_line_pointer = ilp_save;
  
  return op;
}

static char *
sx_parse_reg(char *op, sx_reg_entry *reg)
{
  char *tmp_op;
  int reg_num;
  bfd_boolean is_reg_num_valid;

  /* Remove trailing space */
  op = sx_skip_whitespace(op);
  
  /* extract */
  tmp_op = sx_extract_name(op);
  if (tmp_op == NULL)
    {
      return NULL;
    }
  END_STRING_AND_SAVE(tmp_op);
  
  /* check if the name prefix is a valid register prefix */
  if (strncmp(op, "$s", strlen("$s")) == 0) 
    {
      reg->type = TSREG;
      op += strlen("$s");
    }
  else if (strncmp(op, "$vd", strlen("$vd")) == 0) 
    {
      reg->type = TVDREG;
      op += strlen("$vd");
    }
  else if (strncmp(op, "$vm", strlen("$vm")) == 0) 
    {
      reg->type = TVMREG;
      op += strlen("$vm");
    }
  else if (strncmp(op, "$v", strlen("$v")) == 0)
    {
      reg->type = TVREG;
      op += strlen("$v");      
    }
  else 
    {
      RESTORE_END_STRING(tmp_op);
      return NULL;
    }
  
  /* check if the name suffix is a valid number */
  if (!is_str_number(op)) {
    RESTORE_END_STRING(tmp_op);
    return NULL;
  }
  reg_num = atoi(op);
  
  /* check if a register number is valid for a given register type. */
  switch (reg->type)
    {
      case TSREG:
        is_reg_num_valid = (reg_num >= 0 && reg_num <= 127);
        break;
      case TVREG:
        is_reg_num_valid = (reg_num >= 0 && reg_num <= 7);
        break;
      case TVDREG:
        is_reg_num_valid = (reg_num >= 0 && reg_num <= 63);
        break;
      case TVMREG:
        is_reg_num_valid = (reg_num >= 0 && reg_num <= 15);
        break;
      default:
        is_reg_num_valid = FALSE;
    }
  if (!is_reg_num_valid) {
    RESTORE_END_STRING(tmp_op);
    return NULL;
  }
  reg->num = reg_num;

  RESTORE_END_STRING(tmp_op);
  op = tmp_op;
  return op;
}

static char *
sx_parse_scalar_reg(char *op, sx_reg_entry *scalar_reg)
{
  sx_reg_entry reg;
  char *tmp_op;
  
  tmp_op = sx_parse_reg(op, &reg);
  if (tmp_op != NULL) 
    { 
      /* the index is a register, check if it is a scalar register */ 
      if (reg.type != TSREG) 
        {
          return NULL;
        }
    }
  else 
    {
      return NULL;
    }
    
  *scalar_reg = reg;
  op = tmp_op;
  
  return op;
}

static char *
sx_parse_vector_reg(char *op, sx_reg_entry *vector_reg)
{
  sx_reg_entry reg;
  char *tmp_op;
  
  tmp_op = sx_parse_reg(op, &reg);
  if (tmp_op != NULL) 
    { 
      /* the index is a register, check if it is a vector register */ 
      if (reg.type != TVREG) 
        {
          return NULL;
        }
    }
  else 
    {
      return NULL;
    }
    
  *vector_reg = reg;
  op = tmp_op;
  
  return op;
}

static char *
sx_parse_vector_data_reg(char *op, sx_reg_entry *vector_data_reg)
{
  sx_reg_entry reg;
  char *tmp_op;
  
  tmp_op = sx_parse_reg(op, &reg);
  if (tmp_op != NULL) 
    { 
      /* the index is a register, check if it is a vector data register */ 
      if (reg.type != TVDREG) 
        {
          return NULL;
        }
    }
  else 
    {
      return NULL;
    }
    
  *vector_data_reg = reg;
  op = tmp_op;
  
  return op;
}

static char *
sx_parse_vector_mask_reg(char *op, sx_reg_entry *vector_mask_reg)
{
  sx_reg_entry reg;
  char *tmp_op;
  
  tmp_op = sx_parse_reg(op, &reg);
  if (tmp_op != NULL) 
    { 
      /* the index is a register, check if it is a vector mask register */ 
      if (reg.type != TVMREG) 
        { 
          return NULL;
        }
    }
  else 
    { 
      return NULL;
    }
    
  *vector_mask_reg = reg;
  op = tmp_op;
  
  return op;
}

/* map fixed register to the proper vector register. */
static int
sx_map_fixed_register_to_vector_register(char vreg_type)
{
  int vect_reg_num;
  
  switch (vreg_type)
    {
    case VA0:
      vect_reg_num = 0;
      break;
    case VA1:
      vect_reg_num = 1;
      break;
    case VP0:
      vect_reg_num = 2;
      break;
    case VP1:
      vect_reg_num = 3;
      break;
    case VL0:
      vect_reg_num = 4;
      break;
    case VL1:
      vect_reg_num = 5;
      break;
    case VS0:
      vect_reg_num = 6;
      break;
    case VS1:
      vect_reg_num = 7;
      break;
    default:
      BUG("the vreg_type mapping isn't implemented for %d", vreg_type);
      vect_reg_num = -1;
    }
    
    return vect_reg_num;
} 

static bfd_boolean
sx_is_fixed_register_num_valid(char vreg_type, char value)
{
  bfd_boolean is_valid;
  
  switch (vreg_type)
    {
    case VA0:
    case VP0:
    case VL0:
    case VS0:
      is_valid = (value == 0);
      break;
    case VA1:
    case VP1:
    case VL1:
    case VS1:
      is_valid = (value == 1); 
      break;
    default:
      BUG("the vreg_type checking isn't implemented for %d", vreg_type);
      is_valid = FALSE;    
    }
    
    return is_valid;
} 

/* parses a single fixed vector register (the VS0VS1 and VA0VP1 register pairs
   are broken by the "sx_parse_fixed_vector_reg" function into 2 spearate
   registers). */
static char *
sx_parse_fixed_vector_reg_single(char *op, sx_reg_entry *vector_arithmetic_reg,
				 char vreg_type)
{
  sx_reg_entry reg;
  char *tmp_op;
  bfd_boolean is_valid;
  int reg_num;
  
  /* an ordinary vector register (i.e. $vn) can be used in place of fixed
     vector register. However it has to have a proper number. */
  tmp_op = sx_parse_vector_reg(op, &reg);
  
  if (tmp_op == NULL)
    {
      /* we should try to parse fixed vector register. */
      
      /* Remove trailing space */
      op = sx_skip_whitespace(op);
      
      /* extract */
      tmp_op = sx_extract_name(op);
      if (tmp_op == NULL)
        {
          return NULL;
        }
      END_STRING_AND_SAVE(tmp_op);
      
      /* check if the name prefix is a valid fixed register prefix */
      switch (vreg_type)
        {
        case VA0:
        case VA1:
          is_valid = (strncmp(op, "$va", strlen("$va")) == 0);  
          break;
        case VP0:
        case VP1:
          is_valid = (strncmp(op, "$vp", strlen("$vp")) == 0);
          break;
        case VL0:
        case VL1:
          is_valid = (strncmp(op, "$vl", strlen("$vl")) == 0);
          break;
        case VS0:
        case VS1:
          is_valid = (strncmp(op, "$vs", strlen("$vs")) == 0);
          break;
        default:
          is_valid = FALSE;
        }

      if (!is_valid)
        {
	  RESTORE_END_STRING(tmp_op);
          sx_error("Undefined fixed vector register: %s.", op);
          return NULL;
        }
      
      op += 3;
                  
      /* check if the name suffix is a valid number */
      if (!is_str_number(op))
	{
	  RESTORE_END_STRING(tmp_op);
	  return NULL;
	}
      
      reg_num = atoi(op);
      if (!sx_is_fixed_register_num_valid(vreg_type, reg_num))
        {
	  RESTORE_END_STRING(tmp_op); 
          return NULL;
        }
      
      reg.type = TVREG;
      reg.num = sx_map_fixed_register_to_vector_register(vreg_type);
    
      RESTORE_END_STRING(tmp_op);
    }
    
    /* check if the mapped vector register number matches the one we expect. */
    is_valid = (sx_map_fixed_register_to_vector_register(vreg_type) == 
      reg.num);
    if (!is_valid)
      {
        return NULL;
      }

    *vector_arithmetic_reg = reg;
    op = tmp_op;
    
    return op;
}

/* maps a given primary register to its corresponding alternate register
   (this is used when the "%" instruction suffix is given). */
static char 
sx_alternate_vreg_get(char vreg_type)
{
  char alt_type;
  
  switch (vreg_type)
    {
    case VA0:
      alt_type = VS0;
      break;
    case VA1:
      alt_type = VS1;
      break;
    case VP0:
      alt_type = VL0;
      break;
    case VP1:
      alt_type = VL1;
      break;
    case VL0:
      alt_type = VP0;
      break;
    case VL1:
      alt_type = VP1;
      break;
    case VS0:
      alt_type = VA0;
      break;
    case VS1:
      alt_type = VA1;
      break;
    default:
      BUG("the alternate register doesn't exist for %d", vreg_type);
      alt_type = -1;
    }
    
  return alt_type;
}

static char
sx_vreg_get(char vreg_type, int flags)
{
  int vreg_ret;
  if (flags & FS_SELVU)
    {
      /* select alternate vector arithmetic registers */
      vreg_ret = sx_alternate_vreg_get(vreg_type);
    }
  else
    {
      /* select primary vector arithmetic registers */
      vreg_ret = vreg_type;
    }
    
    return vreg_ret;
}

static char
sx_select_vreg_to_parse(char vreg_type, int flags, int iter)
{
  char vreg_return = 0;

  if (vreg_type == VS0VS1)
    {
      /* we are dealing with the "$vs1, $vs2" operands in the same field. */
      if (iter == 0)
        {
          /* in the first iteration we parse VS0 register. */
          vreg_return = sx_vreg_get(VS0, flags);
        }
      else if (iter == 1)
        {
          /* in the second iteration we parse VS1 register. */
          vreg_return = sx_vreg_get(VS1, flags);
        }
    }
  else if (vreg_type == VA0VP1)
    {
      /* we are dealing with the "$va0, $vp1" operands in the same field. */
      if (iter == 0)
        {
          /* in the first iteration we parse VA0 register. */
          vreg_return = sx_vreg_get(VA0, flags);
        }
      else if (iter == 1)
        {
          /* in the second iteration we parse VP1 register. */
          vreg_return = sx_vreg_get(VP1, flags);
        }
    }
  else
    {
      /* every other vreg type has only one fixed vector register per operand
         field. */
      vreg_return = sx_vreg_get(vreg_type, flags);
    }
    
  return vreg_return;
}

static char *
sx_parse_fixed_vector_reg(char *op, char vreg_type, int flags)
{
  char tmp_vreg_type;
  int num_fixed_reg;
  char *tmp_op;
  int i;
  sx_reg_entry vector_arithmetic_reg;
  
  if (vreg_type == VS0VS1 || vreg_type == VA0VP1)
    {
      num_fixed_reg = 2;
    }
  else if (vreg_type != 0)
    {
      num_fixed_reg = 1;
    }
  else 
    {
      num_fixed_reg = 0;
    }
  
  tmp_op = op;
  for (i=0; i<num_fixed_reg; i++)
    {      
      /* parse the fixed vectro arithmetic register */
      tmp_vreg_type = sx_select_vreg_to_parse(vreg_type, flags, i);
      tmp_op = sx_parse_fixed_vector_reg_single(tmp_op, &vector_arithmetic_reg, 
        tmp_vreg_type);
      if (tmp_op == NULL)
        {
          return NULL;
        }
        
      if (num_fixed_reg == 2 && i == 0)
        {
          /* before you proceed with the next operand, check for ',' */
          tmp_op = sx_parse_comma(tmp_op);
          if (tmp_op == NULL) 
            {
              return NULL;
            }
        }
    }
    
    return tmp_op;
}

/* parse vector result: ($vn, $vdm),
 
      $vn = vector register
      $vdm = vector data register */
static char *
sx_parse_vector_result_both(char *op, sx_vres_entry *vec_res_reg)
{
  char *tmp_op;
  sx_vres_entry reg;
  
  /* check for '(' */
  if (*op != '(') 
    {
      return NULL;
    }
  op++;
 
  /* parse $vn (vector arithmetic register). */
  tmp_op = sx_parse_vector_reg(op, &reg.vreg);
  if (tmp_op == NULL)
    {
      return NULL;
    }

  op = tmp_op;
    
  /* check for ',' */
  if (*op != ',') 
    {
      return NULL;
    }
  op++;
  
  /* parse $vdm (vector data register). */
  tmp_op = sx_parse_vector_data_reg(op, &reg.vdreg);
  if (tmp_op == NULL)
    {
      return NULL;
    }

  op = tmp_op;


  /* check for ')' */
  if (*op != ')') 
    {
      return NULL;
    }
  op++;
  
  reg.type = VRES_BOTH;
  *vec_res_reg = reg;
  
  return op;
}

/* parse vector result: [ $vx | $vdz | ($vx, $vdz) ],
 
      $vx = vector register
      $vdz = vector data register */
static char *
sx_parse_vector_result(char *op, sx_vres_entry *vec_res_reg)
{
  char *tmp_op;
  sx_reg_entry reg;
  
  if (*op == '(')
    {
      /* ($vx, $vdz) vectors are present. */
      tmp_op = sx_parse_vector_result_both(op, vec_res_reg);
      if (tmp_op == NULL)
        {
          return NULL;          
        }
    }
  else 
    {
      if ((tmp_op = sx_parse_vector_reg(op, &reg)) != NULL)
        {
          /* only $vx vector is present. */
          vec_res_reg->type = VRES_VREG;
          vec_res_reg->vreg = reg;
        }
      else if ((tmp_op = sx_parse_vector_data_reg(op, &reg)) != NULL)
        {
          /* only $vdz vector is present. */
          vec_res_reg->type = VRES_VDREG;
          vec_res_reg->vdreg = reg;
        }
      else
        {
          return NULL;
        }
    }
  
  op = tmp_op;
    
  return op;
}


/* parse immediate value. According to SX-8 "Assembly Language Reference
   Manual", immediate value is an integer value: -64 <= imm_value <= 63 */
static char *
sx_parse_immediate(char *op, char *immediate, enum sx_immediate_type imm_type)
{
  char *tmp_op;
  expressionS exp;
  bfd_boolean is_range_valid;
  char tmp_imm;
  
  tmp_op = sx_parse_expression(op, &exp, expr_normal);
  if (tmp_op == NULL || exp.X_op != O_constant) {
    return NULL;
  }
  
  switch (imm_type)
    {
    case IMM_TYPE_I:
      is_range_valid = (exp.X_add_number >= -64 && 
        exp.X_add_number <= 63);
      tmp_imm = (char)exp.X_add_number;
      break;
    case IMM_TYPE_N:
      is_range_valid = (exp.X_add_number >= 0 &&
        exp.X_add_number <= 128);
      /* in SX object files, 128 is encoded as 0x00. */
      tmp_imm = ((int)exp.X_add_number % 128);
      break;
    default:
      is_range_valid = FALSE;
    }
  
  if(!is_range_valid) 
    {
      return NULL;
    }
  
  *immediate = tmp_imm;
  
  return tmp_op;

}

/* parse conditional code. According to SX-8 "Assembly Language Reference 
   Manual", immediate value is an integer operand whose value satisfies the 
   following rule: 0x0 <= cond_code <= 0xf */
static char *
sx_parse_conditional_code(char *op, char *cond_code)
{
  char *tmp_op;
  expressionS exp;
  
  tmp_op = sx_parse_expression(op, &exp, expr_normal);
  if (tmp_op == NULL || exp.X_op != O_constant) {
    return NULL;
  }
    
  if((int)exp.X_add_number < 0x0  || (int)exp.X_add_number > 0xf) 
    {
      return NULL;
    }
  
  *cond_code = (char)exp.X_add_number;
  
  return tmp_op;
}

/* based on the opcode, check if software controlled value falls into the
   given range. */
static bfd_boolean
sx_check_software_controlled_range(unsigned char opcode, int value)
{
  bfd_boolean is_valid;
  
  switch (opcode)
    {
    case O_FIX:
    case O_FIXX:
    case O_LSR:
    case O_SSR:
    case O_RET:
    case O_LVM:
    case O_SVM:
    case O_VFIX:
    case O_VFIXX:
      is_valid = (value == 0 || value == 1); 
      break;
    case O_LMIR:
    case O_SMIR:
      is_valid = (value >= 0 && value <= 31);
      break;
    case O_FIDCR:
      is_valid = (value >= 0 && value <= 3);
      break;
    case O_MONC:
      is_valid = (value >= 0 && value <= 255);
      break;
    case O_VLDX:
    case O_VLDUX:
    case O_VLDLX:
      is_valid = (value >= -64 && value <= 63 && (value % 8) == 0);
      break;
    default:
      BUG("the range checking isn't implemented for %d", opcode);
      is_valid = FALSE;
    }

    return is_valid;
}

/* parse software controlled field. this is a type of immediate value whose
   range depends on instruction type. */
static char *
sx_parse_software_controlled(char *op, char *immediate, unsigned char opcode)
{
  char *tmp_op;
  expressionS exp;
  bfd_boolean is_range_valid;
  
  tmp_op = sx_parse_expression(op, &exp, expr_normal);
  if (tmp_op == NULL || exp.X_op != O_constant) {
    return NULL;
  }

  is_range_valid = sx_check_software_controlled_range(opcode, 
    exp.X_add_number);
  if(!is_range_valid) 
    {
      return NULL;
    }
  
  *immediate = exp.X_add_number;
  
  return tmp_op;
}

/* parse mask. According to chapter 7.4: "Immediate Value/Conditional 
   Code Notation" of SX "Assembly Language Reference Manual", the mask is 
   defined, as follows:
   * (m)0: 64-bit immediate value having m zeroes from left and (64-m) ones.
   * (m)1: 64-bit immediate value having m ones from left and (64-m) zeroes. */
static char *
sx_parse_mask(char *op, sx_imm_mask *mask)
{
  char *tmp_op;
  expressionS exp;
  sx_imm_mask tmp_mask;
  
  /* remove trailing spaces. */
  op = sx_skip_whitespace(op);

  if (*op != '(') 
    {
      return NULL;
    }
  
  /* skip '(' character. */
  op++;
  
  /* parse an integer constant. */
  tmp_op = sx_parse_expression(op, &exp, expr_normal);
  if (tmp_op == NULL || exp.X_op != O_constant)
    { 
      return NULL;
    }
  
  if ((int)exp.X_add_number < 0 || (int)exp.X_add_number > 64)
    {
      return NULL;
    }
  
  tmp_mask.m = (int)exp.X_add_number;
  
  op = tmp_op;
  
  if (*op != ')') 
    {
      return NULL;
    }
  
  /* skip ')' character. */
  op++;
  
  /* parse the mask type. */
  if (*op == '0')
    {
      tmp_mask.i = 0;
    }
  else if (*op == '1')
    {
      tmp_mask.i = 1;
    }
  else
    {
      return NULL;
    }
  
  /* skip the mask type. */
  op++;
  
  *mask = tmp_mask;
  
  return op; 
}

/* parses address syllable (for the definition of address syllable, look at 
   the SX "Assembly Language Reference Manual", chapter 7.3: "Effective Address
   Notation").*/
static char *
sx_parse_address_syllable(char *op, sx_asx_entry *asx,
			  bfd_boolean is_index_enabled)
{
  char *tmp_op;
  
  expressionS exp;
  sx_reg_entry reg;
  char immediate;
  bfd_boolean disp_present = FALSE, index_present = FALSE, 
    base_present = FALSE;

  asx->displ.X_op = O_illegal;
  asx->base.type = -1;
  asx->index.type = SX_IDX_NONE;

  /* Remove trailing spaces */
  op = sx_skip_whitespace(op);

  if (*op != '(') 
    { 
      /* the displacement is present */
      disp_present = TRUE;
      
      tmp_op = sx_parse_expression(op, &exp, expr_evaluate);
      if (tmp_op == NULL) 
        {
          return NULL;
        }
      op = tmp_op;

      if(exp.X_op == O_symbol && !using_set)
	{
	  /* if label is specified as displacement, base register must
	     be set with using pseudo-op */
	  sx_error("Base register should be set with the using pseudo-op "
		 "when a label is specified for displacement.");
	  return NULL;
	}
      else if(exp.X_op == O_symbol && using_set)
	{
	  base_present = TRUE;
	  asx->base = using_base_reg;

	  /* when a label L was used for displacement (and base register was set
	     to label U with using command), the actual displacement is computed
	     as L-U.
	  */
	  exp.X_op_symbol = using_base_val.X_add_symbol;
	  exp.X_op = O_subtract;
	  asx->displ = exp;            
	}
      else if(exp.X_op != O_illegal)
	{
	  asx->displ = exp;
	}
      else
	{
	  sx_error("Illegal expression specified for displacement.");
	  return NULL;
	}
    }
  else
    {
      /* no displacement: 0 implied */
      asx->displ.X_op = O_constant;
      asx->displ.X_add_number = 0;
    }

  if (*op == END_OF_INSN)
    {
      if (disp_present) 
        {
          /* we only have a displacement, we don't need to check for index
	     and base */
          return op;
        }
      else
        { 
          return NULL;
        }
    }
    
  /* we expect the rest of the syllable to be of the ([index,][base]) form*/ 
  if (*op != '(') 
    {
      return NULL;
    }

  /* skip the '(' character */  
  ++op;
  

  if (*op != ',') 
    { 
      /* index should come next (index is either immediate data or a scalar
       * register) */
      index_present = TRUE;

      if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
        {
          /* the index is scalar register */
          asx->index.type = SX_IDX_REG;
          asx->index.data.scalar_reg = reg;
        }
      else if ((tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_I)) !=
	       NULL)
        {
          /* the index is immediate data */
          asx->index.type = SX_IDX_IMM;
          asx->index.data.imm = (int)immediate;
        } 
      else
        {
          return NULL;
        }
      
      op = tmp_op;
    }

  if (*op == ',')
    {
      /* the base should come next (base can only be a scalar register) */
      base_present = TRUE;

      /* skip the ',' character */  
      ++op;
      
      op = sx_skip_whitespace(op);
      
      /* The rest must be the base register */      
      tmp_op = sx_parse_scalar_reg(op, &reg);
      if (tmp_op == NULL)
        {
          return NULL;
        }
       
      op = tmp_op;
      
      asx->base = reg;

      /* when explicit base is given, displacement *may not* be
	 a label! */
      if(disp_present && asx->displ.X_op == O_symbol)
	{
	  sx_error("A label may not be specified for displacement when "
		 "an explicit base register is given.");
	  return NULL;
	}
    }
  
  if (*op == ')')
    {
      /* if we encounter ')' char, this means the address syllable has ended */
      
      /* skip the ')' character */  
      ++op;
    }
  else 
    {
      return NULL;
    }
  
  /* if we use brackets, either index or base have to be present */
  if (!index_present && !base_present)
    {
      /* when using brackets, either index or base have to be specified */
      return NULL;
    }
    
  /* the condition below differentiates between AS and ASX address 
     syllables. */
  if (!is_index_enabled && index_present)
    {
      /* index given, but only AS expected */
      return NULL;
    }
  
  return op;
}

/* Parse an unidentified operand */

/* TODO: actually, I want to get rid of this - we should never encounter an
   "unindentified" operand. */
void
md_operand (expressionS *e ATTRIBUTE_UNUSED)
{
  BUG("md_operand() should never be called.");
}


/*****************************************************************************/
/* Parsing: instruction formats                                              */
/*****************************************************************************/

static char *
sx_parse_operands_rx(char *op)
{
  /* the default form of operands for RX-format assembler instructions is, as
     follows:
       $sx, ASX
       
       $sx = scalar register (0 <= n <= 127).
       ASX = address syllable (index specification enabled) */
  
  sx_reg_entry reg;
  sx_asx_entry asx;
  int op_num = 0;
  char *tmp_op;
  
  /* parse $sx (scalar register). */
  tmp_op = sx_parse_scalar_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("parsing register ident failed");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;
  
  /* parse ASX (address syllable). */
  tmp_op = sx_parse_address_syllable(op, &asx, TRUE);
  if (tmp_op == NULL) 
    {
      sx_error("parsing address syllable failed");
      return NULL;
    }
  
  op = tmp_op;
  inst.ops[op_num].type = OP_ASX;
  inst.ops[op_num].op.asx = asx;
  op_num++;

  inst.opcount = op_num;
  
  return op;
}

static char *
sx_parse_operands_cfx(char *op)
{
  /* there are 3 forms of CFX-format assembler instructions:
     1) the most common form is: [$sy | I], AS
     2) the "bc", "bcs", "bcf" instructions have the following form:
        C, [$sy | I], AS
     3) the "b", "bs", "bf" instructions have the following form:
        AS
       
       $sy = scalar register (0 <= n <= 127).
       AS = address syllable (index specification disabled) 
       C = condition code
       I = immediate value */
  
  unsigned char condf;
  char *tmp_op;
  int op_num = 0;
  char immediate;
  expressionS exp;
  sx_reg_entry reg;
  sx_asx_entry asx;

  /* first we need to determine conditional flags */
  condf = sx_get_condf(inst.op);
  if(CONDF_INVALID == condf)
    {
      sx_error("Unable to determine conditional flags for mnemonic %s",
	     inst.op->name);
      return NULL;
    }
  else if(CONDF_FALSE == condf)
    {
      /* we have encountered one of the following instructions: "bc", "bcs", 
         "bcf". These instructions have the following form: C, [$sy | I], AS */
      
      /* we expect conditional flags as the first parameter: 0 <= cf <= 0x0f */
      tmp_op = sx_parse_expression(op, &exp, expr_normal);
      if (tmp_op != NULL && exp.X_op == O_constant)
        {
          if((int)exp.X_add_number < 0 || (int)exp.X_add_number > 0xF)
            {
              sx_error("Expected a conditional flag (0 <= CF <= 0xF), got %x",
                (int)exp.X_add_number);
              return NULL;
            }
          condf = (unsigned char)exp.X_add_number;
          inst.ops[op_num].type = OP_IMM;
          inst.ops[op_num].op.imm = (char)condf;
          op_num++;
        }
      else
        {
          sx_error("Only constants are allowed for conditional flags.");
        }  

      /* check for ',' */
      tmp_op = sx_parse_comma(tmp_op);
      if (tmp_op == NULL) 
        {
          sx_error("Missing operands.");
          return NULL;
        }
      op = tmp_op;
    }
  else
    {
      /* condf implied by mnemonic */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = condf;
      op_num++;
    } 


  if(inst.op->field.y != 0)
    {

      /* value to test: parse scalar register or immediate */

      /* so, is it a sreg? */
      if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
	{
	  /* yup ... register */
	  inst.ops[op_num].type = OP_REG;
	  inst.ops[op_num].op.reg = reg;
	}
      else if ((tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_I)) !=
	       NULL) 
	{
	  /* nope. must be an immediate */
	  inst.ops[op_num].type = OP_IMM;
	  inst.ops[op_num].op.imm = immediate;
	}
      else
	{
	  sx_error("Register or an immediate value expected.");
	  return NULL;
	}
      
      op_num++;
      op = tmp_op;  
      
      /* check for ',' */
      tmp_op = sx_parse_comma(op);
      if (tmp_op == NULL) 
	{
	  sx_error("Missing operands.");
	  return NULL;
	  
	}
      op = tmp_op;
    }
  else
    {
      /* unconditional branch ("b", "bs", "bf"): no need for value to test */
    }
  
  /* parse AS address syllable */
  tmp_op = sx_parse_address_syllable(op, &asx, FALSE);
  if (tmp_op == NULL) 
    {
      sx_error("Address syllable (index specification disabled) expected.");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_ASX;
  inst.ops[op_num].op.asx = asx;
  op_num++;

  inst.opcount = op_num;
  
  return op; 
}

static char *
sx_parse_operands_rw(char *op)
{
  /* the form of operands for RW-format assembler instructions is, as
     follows:
       $sx, [$sy | I], [$sz | M]
       
       $sx, $sy, $sz = scalar register (0 <= n <= 127).
       I = immediate value 
       M = mask */

  char *tmp_op;
  int op_num = 0;
  char immediate;
  sx_reg_entry reg;
  sx_imm_mask mask;

  /* parse $sx (scalar register). */
  tmp_op = sx_parse_scalar_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("Expecting scalar register operand.");
      return NULL;
    }
  else if (inst.op->opcode != O_FCQ && !sx_is_register_even(&reg))
    {
      /* the number of X scalar register should be an even number for the 
         RW-type instructions (except for the "fcq" instruction). */
      sx_error("Expecting even-numbered scalar register.");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("Missing operands.");
      return NULL;
    }
  op = tmp_op;

  /* parse [$sy | I] (scalar register or immediate value). */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      /* yup ... register */

      /* the number of the scalar register should be an even number for the 
         RW-type instructions (except for the "fmq" instruction). */      
      if (inst.op->opcode != O_FMQ && !sx_is_register_even(&reg))
        {
          sx_error("Expecting even-numbered scalar register.");
          return NULL;
        }

      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
      op_num++;
    }
  else if ((tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_I)) != NULL) 
    {
      /* nope. must be an immediate */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = immediate;
      op_num++;
    }
  else
    {
      sx_error("Register or an immediate value expected for testing.");
      return NULL;
    }

  op = tmp_op;  
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("Missing operands.");
      return NULL;
    
    }
  op = tmp_op;

  /* parse [$sz | M] (scalar register or mask). */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      /* scalar register encountered. */
      
      /* the number of the scalar register should be an even number for the 
         RW-type instructions (except for the "fmq" instruction). */      
      if (inst.op->opcode != O_FMQ && !sx_is_register_even(&reg))
        {
          /* the number of scalar register should be an even number for all the 
             RW-type instructions. */
          sx_error("Expecting even-numbered scalar register.");
          return NULL;
        }
      
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
      op_num++;
    }
  else if ((tmp_op = sx_parse_mask(op, &mask)) != NULL) 
    {
      /* mask encountered. */
      inst.ops[op_num].type = OP_MASK;
      inst.ops[op_num].op.mask = mask;
      op_num++;
    }
  else
    {
      sx_error("Scalar register or an mask expected for testing.");
      return NULL;
    }

  op = tmp_op;

  inst.opcount = op_num;
    
  return op; 
}

static char *
sx_parse_operands_rz(char *op)
{
  /* the form of operands for RW-format assembler instructions is, as
     follows:
       $sx, N, AS
       
       $sx = scalar register (0 <= n <= 127).
       N = immediate value 
       AS = address syllable (index specification disabled) */

  sx_reg_entry reg;
  sx_asx_entry asx;
  char immediate;
  int op_num = 0;
  char *tmp_op;
  
  /* parse $sx (scalar register). */
  tmp_op = sx_parse_scalar_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("Parsing of register ident failed.");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;
  
  /* parse N (immediate value) */
  tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_N);
  if (tmp_op == NULL) 
    {
      sx_error("Immediate value expected.");
      return NULL;
    }
    
  inst.ops[op_num].type = OP_IMM;
  inst.ops[op_num].op.imm = immediate;
  op_num++;

  op = tmp_op;  

  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;
  
  /* parse AS (address syllable). */
  tmp_op = sx_parse_address_syllable(op, &asx, FALSE);
  if (tmp_op == NULL) 
    {
      sx_error("Parsing of address syllable failed");
      return NULL;
    }
  
  op = tmp_op;
  inst.ops[op_num].type = OP_ASX;
  inst.ops[op_num].op.asx = asx;
  op_num++;

  inst.opcount = op_num;
  
  return op;
}

static char *
sx_parse_operands_rr_fm_dxy_z(char *op)
{
  char *tmp_op;
  sx_reg_entry reg;
  int op_num = 0;
  char immediate;
  sx_imm_mask mask;
  
  /* parse $vx (vector register). */
  tmp_op = sx_parse_vector_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("Parsing of register ident failed");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for '(' */
  if (*op != '(') 
    {
      sx_error("'(' expected but not found");
      return NULL;
    }
  op++;

  /* parse [$sy | N] (scalar register or immediate value). */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      /* scalar register encountered. */
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
    }
  else if ((tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_N)) != NULL) 
    {
      /* immediate encountered. */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = immediate;
    }
  else
    {
      sx_error("Register or an immediate value expected for testing.");
      return NULL;
    }

  op_num++;
  op = tmp_op;  
  
  /* check for ')' */
  if (*op != ')') 
    {
      sx_error("')' expected but not found");
      return NULL;
    }
  op++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;
  
  /* parse [$sz | M] (scalar register or mask). */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      /* scalar register encountered. */
      
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
    }
  else if ((tmp_op = sx_parse_mask(op, &mask)) != NULL) 
    {
      /* mask encountered. */
      inst.ops[op_num].type = OP_MASK;
      inst.ops[op_num].op.mask = mask;
    }
  else
    {
      sx_error("Scalar register or an mask expected.");
      return NULL;
    }

  op = tmp_op;  
  op_num++;
  
  inst.opcount = op_num;
  
  
  return op;
}

static char *
sx_parse_operands_rr_fm_dx_zy(char *op)
{
  char *tmp_op;
  sx_reg_entry reg;
  int op_num = 0;
  char immediate;

  /* parse $sx (scalar register). */
  tmp_op = sx_parse_scalar_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("parsing register ident failed");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;

  /* parse $vz (vector register). */
  tmp_op = sx_parse_vector_reg(op, &reg);
  if (tmp_op == NULL) 
    {
      sx_error("parsing register ident failed");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_REG;
  inst.ops[op_num].op.reg = reg;
  op_num++;
  
  /* check for '(' */
  if (*op != '(') 
    {
      sx_error("'(' expected but not found");
      return NULL;
    }
  op++;

  /* parse [$sy | N] (scalar register or immediate value). */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      /* scalar register encountered. */
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
    }
  else if ((tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_N)) != NULL) 
    {
      /* immediate encountered. */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = immediate;
    }
  else
    {
      sx_error("Register or an immediate value expected for testing.");
      return NULL;
    }

  op = tmp_op;  
  
  /* check for ')' */
  if (*op != ')') 
    {
      sx_error("')' expected but not found");
      return NULL;
    }
  op++;

  op_num++;
    
  inst.opcount = op_num;
  
  return op;
}

static void
sx_opt_param_default(int parno, unsigned short fflags ATTRIBUTE_UNUSED)
{
  /* currently, optional parameter is always a literal
     and default value is always zero */
  inst.ops[parno].type = OP_IMM;
  inst.ops[parno].op.imm = 0;
}

static char *
sx_parse_operands_rr_rv_general(char *op)
{
  /* the RR and RV-format assembler instructions have many forms, which differ 
     significantly from each other. This is why we have to use the information
     from "sx_opcode" table in order to make this parsing function as general 
     as possible. */

  unsigned short op_types[MAX_NUM_OPERANDS];
  char op_types_vreg[MAX_NUM_OPERANDS];
  int i;
  char *tmp_op;
  int op_num = 0;
  sx_reg_entry reg;
  sx_vres_entry vec_res_reg;
  sx_imm_mask mask;
  char immediate;
  char cond_code;
  bfd_boolean comma_exists = FALSE;
  bfd_boolean inc_operand_index;
    
  /* first get order of the X,Y,Z fileds */
  if (SX_OA_FMT(inst.op->format) == FM_XZY) 
    {
      /* the fields are in the following order: "X,Z,Y" */
      op_types[0] = inst.op->field.x;
      op_types[1] = inst.op->field.z;
      op_types[2] = inst.op->field.y;
    }
  else if (SX_OA_FMT(inst.op->format) == FM_ZX)
    {
      /* the fields are in the following order: "Z,X" */
      op_types[0] = inst.op->field.z;
      op_types[1] = inst.op->field.x;
      op_types[2] = 0;
    }
  else if (SX_OA_FMT(inst.op->format) == FM_DXZ_Y || 
	   SX_OA_FMT(inst.op->format) == FM_XYZ) 
    {
      /* the fields are in the following order: "X,Y,Z" (this is true even
         for FM_DXZ_Y instructions since we parse (X,Z) vector result as a 
         whole) and other operands after that. */
      op_types[0] = inst.op->field.x;
      op_types[1] = inst.op->field.y;
      op_types[2] = inst.op->field.z;
    }
  else
    {
      BUG("Unexpected operand order in RR/RV-type instruction.");
      return NULL;
    }
    
  /* then, get the order for fixed registers */
  op_types_vreg[0] = inst.op->vreg.x;
  op_types_vreg[1] = inst.op->vreg.y;
  op_types_vreg[2] = inst.op->vreg.z;

  /* parse the fields, one by one */
  for (i = 0; i < MAX_NUM_OPERANDS; i++)
    {
      /* skip the operand:
         1) that doesn't have the type set. 
         2) whose type equals to (TVDZC|TVDREG). This is a Z-field part of
            vector result, which is of no use when doing the parsing (
            the X-field part of vector result was already used) */
      if ((op_types[i] == 0 && op_types_vreg[i] == 0) || 
	  op_types[i] == (TVDZC|TVDREG))
        {
          continue;
        }
      else if (comma_exists)
        {
          /* before you proceed with the next operand, check for ',' */
          tmp_op = sx_parse_comma(op);
          if (tmp_op == NULL) 
            {
	      /* hmmm, is this an optional operand? */
	      if(op_types[i] & TOPT)
		{
		  sx_opt_param_default(op_num, op_types[i]);
		  op_num++;
		  continue;
		}
	      else
		{
		  sx_error("Missing operand.");
		  return NULL;
		}
            }
          op = tmp_op;
          comma_exists = FALSE;
        }
        
      inc_operand_index = TRUE;
      if ((op_types[i] & (TVDVC|TVREG)) == (TVDVC|TVREG) &&
	  (tmp_op = sx_parse_vector_result(op, &vec_res_reg)) != NULL)
        {
          /* we extracted vector result (vector register, vector data register 
             or both) */
          inst.ops[op_num].type = OP_VRES;
          inst.ops[op_num].op.vec_res_reg = vec_res_reg;

          /* sometimes, a fixed vector arithmetic register coesxists with the 
             vector result in the same field. In such cases, we have to parse
             fixed register as well. */
          if (op_types_vreg[i] != 0)
            {
              op = tmp_op;
              /* before you proceed with the next operand, check for ',' */
              tmp_op = sx_parse_comma(op);
              if ((NULL == (tmp_op = sx_parse_comma(op))) ||
		  (NULL == (tmp_op = sx_parse_fixed_vector_reg(tmp_op, 
							       op_types_vreg[i], inst.flags)))) 
                {
                  sx_error("Missing fixed vector arithmetic register.");
                  return NULL;
                }
              op = tmp_op;              
            }
          
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TSREG) == TSREG &&
	       (tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
        {
          /* we extracted the scalar register. */
          inst.ops[op_num].type = OP_REG;
          inst.ops[op_num].op.reg = reg;
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TVREG) == TVREG && op_types_vreg[i] == 0 &&
	       (tmp_op = sx_parse_vector_reg(op, &reg)) != NULL)
        {
          /* we extracted the vector register. */
          inst.ops[op_num].type = OP_REG;
          inst.ops[op_num].op.reg = reg;
          comma_exists = TRUE;
        }        
      else if ((op_types[i] & TVREG) == TVREG && op_types_vreg[i] != 0 &&
	       (tmp_op = sx_parse_fixed_vector_reg(op, op_types_vreg[i],
						   inst.flags)) != NULL)        
        {
          /* we extracted the fixed vector arithmetic register */
          
          /* since the implicit register does not generate code, we shouldn't 
             store its data */
          
          comma_exists = TRUE;
          inc_operand_index = FALSE;
        }
      else if ((op_types[i] & TVMREG) == TVMREG &&
	       (tmp_op = sx_parse_vector_mask_reg(op, &reg)) != NULL)
        {
          /* we extracted the vector mask register. */
          inst.ops[op_num].type = OP_REG;
          inst.ops[op_num].op.reg = reg;
          comma_exists = TRUE; 
        }
      else if ((op_types[i] & TVDREG) == TVDREG &&
	       (tmp_op = sx_parse_vector_data_reg(op, &reg)) != NULL)
        {
          /* we extracted the vector data register. */
          inst.ops[op_num].type = OP_REG;
          inst.ops[op_num].op.reg = reg;
          comma_exists = TRUE; 
        }
      else if ((op_types[i] & TLIT_MASK) == TLIS63 &&
	       (tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_I)))
        {
          /* we extracted the immediate value with values between 
             -64 and 63. */
          inst.ops[op_num].type = OP_IMM;
          inst.ops[op_num].op.imm = immediate;
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TLIT_MASK) == TLM0M1 &&
	       (tmp_op = sx_parse_mask(op, &mask)) != NULL)
        {
          /* mask encountered. */
          inst.ops[op_num].type = OP_MASK;
          inst.ops[op_num].op.mask = mask;
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TLIT_MASK) == TLI127 &&
	       (tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_N)))
        {
          /* we extracted the immediate value with values between 
             1 and 128. */
          inst.ops[op_num].type = OP_IMM;
          inst.ops[op_num].op.imm = immediate;
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TSOFT) == TSOFT &&
	       (tmp_op = sx_parse_software_controlled(op, &immediate, 
						      inst.op->opcode)) != NULL)
        {
          /* we extracted the immediate value with values between 
             1 and 128. */
          inst.ops[op_num].type = OP_IMM;
          inst.ops[op_num].op.imm = immediate;
          comma_exists = TRUE;
        }
      else if ((op_types[i] & TCONDF) == TCONDF &&
	       (tmp_op = sx_parse_conditional_code(op, &cond_code)) != NULL)
        {
          inst.ops[op_num].type = OP_IMM;
          inst.ops[op_num].op.imm = cond_code;
          comma_exists = TRUE;          
        }
      else 
        {
	  /* hmmm, is the first operand optional? */
	  if(op_types[i] & TOPT)
	    {
	      sx_opt_param_default(op_num, op_types[i]);
	      tmp_op = op;
	    }
	  else
	    {
	      sx_error("Failed to parse operand.");
	      return NULL;
	    }
        }
      
      if (inc_operand_index)
        op_num++;
      
      op = tmp_op;        
    }

  inst.opcount = op_num;
    
  return op;
}

static char *
sx_parse_operands_rr(char *op)
{
  /* the RR-format assembler instructions have many forms, which differ 
     significantly from each other. This is why we have to use the information
     from "sx_opcode" table in order to make this parsing function as general 
     as possible. */

  /* we parse the most exotic forms of RR-type instructions in a separate
     functions. */
  if (SX_OA_FMT(inst.op->format) == FM_DXY_Z) 
    {
      /* parse "X(Y),Z"-shape operands*/
      return sx_parse_operands_rr_fm_dxy_z(op);
    }
  else if (SX_OA_FMT(inst.op->format) == FM_DX_ZY) 
    {
      /* parse "X,Z(Y)"-shape operands*/
      return sx_parse_operands_rr_fm_dx_zy(op);
    }
 
  /* the majority of RR instructions are parsed by the general function. */
  return sx_parse_operands_rr_rv_general(op);
}


static char *
sx_parse_operands_rrx(char *op)
{
  /* there are 2 forms of RRX-format assembler instructions:
     1) the most common form is: 
        [$vx | $vdx | ($vx,$vdx)], [$sy | N], [$sz | M]
     2) the "vsfa" instruction has the following form:
        [$vx | $vdx | ($vx,$vdx)], vl1, [$sy | N], [$sz | M]
       
       $vx = vector register (0 <= n <= 7)
       $vdx = vector data register (0 <= n <= 63)
       $sy, $sz = scalar register (0 <= n <= 127).
       $vl1 = fixed logical vector register (it maps to $v5) 
       $N = immediate value 
       $M = mask */
       
  char *tmp_op;
  int op_num = 0;
  char immediate;
  sx_reg_entry reg;
  sx_vres_entry vec_res_reg;
  sx_imm_mask mask;
  int vreg_type;
  
  /* parse [$vx | $vdx | ($vx,$vdx)] */
  tmp_op = sx_parse_vector_result(op, &vec_res_reg);
  if (tmp_op == NULL)
    {
      sx_error("Failed to parse vector result registers.");
      return NULL;
    }
  
  op = tmp_op;  
  inst.ops[op_num].type = OP_VRES;
  inst.ops[op_num].op.vec_res_reg = vec_res_reg;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;
  
  if ((inst.op->field.x & TREG_MASK) == TVREG && inst.op->vreg.x == VL1)
  {
    /* we are parsing an instruction of the following form:
       [$vx | $vdx | ($vx,$vdx)], vl1, [$sy | N], [$sz | M] */
    vreg_type = sx_vreg_get(VL1, inst.flags);
    tmp_op = sx_parse_fixed_vector_reg_single(op, &reg, vreg_type);
    if (tmp_op == NULL)
      {
        sx_error("Failed to parse the 'vl1' register.");
        return NULL;
      }
      
    op = tmp_op;
    
    /* since the implicit register does not generate code, we don't have to 
       store its data */
    
    /* check for ',' */
    tmp_op = sx_parse_comma(op);
    if (tmp_op == NULL) 
      {
        sx_error("',' expected but not found.");
        return NULL;
      }
    op = tmp_op;    
  }

  /* parse [$sy | N] (scalar register or immediate value) */  
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
    }
  else if ((inst.op->field.y & TLIS63) == TLIS63 && 
    (tmp_op = sx_parse_immediate(op, &immediate, IMM_TYPE_I)) != NULL)
    {
      /* the immediate value doesn't have additional limitations */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = immediate;
    }
  else if ((inst.op->field.y & TSOFT) == TSOFT && 
    (tmp_op = sx_parse_software_controlled(op, &immediate, inst.op->opcode)) != 
    NULL)
    {
      /* the immediate value doesn't have additional limitations */
      inst.ops[op_num].type = OP_IMM;
      inst.ops[op_num].op.imm = immediate;
    }
  else
    {
      sx_error("Register or an immediate value expected.");
      return NULL;
    }  
  
  op = tmp_op;
  op_num++;
  
  /* check for ',' */
  tmp_op = sx_parse_comma(op);
  if (tmp_op == NULL) 
    {
      sx_error("',' expected but not found");
      return NULL;
    }
  op = tmp_op;

  /* parse [$sz | M] (scalar register or mask) */
  if ((tmp_op = sx_parse_scalar_reg(op, &reg)) != NULL)
    {
      inst.ops[op_num].type = OP_REG;
      inst.ops[op_num].op.reg = reg;
    }
  else if ((tmp_op = sx_parse_mask(op, &mask)) != NULL)
    {
      inst.ops[op_num].type = OP_MASK;
      inst.ops[op_num].op.mask = mask;
    }
  else
    {
      sx_error("Register or mask expected.");
      return NULL;
    }  

  op = tmp_op;
  op_num++;
  
  inst.opcount = op_num;

  return op;
}


/***
 * general operand parsing
 */
static char *
sx_parse_operands(char *op)
{
  /* we decide on what to parse based on instruction format
     from the opcode info for the just-parsed mnemonic, stored
     in inst.op */
  if (inst.op->format & F_RR)
    {
      return sx_parse_operands_rr(op);
    }
  else if(inst.op->format & F_RX) 
    {
      return sx_parse_operands_rx(op);
    }
  else if(inst.op->format & F_CFX) 
    {
      return sx_parse_operands_cfx(op);
    }
  else if (inst.op->format & F_RW)
    {
      return sx_parse_operands_rw(op);
    }
  else if (inst.op->format & F_RZ)
    {
      return sx_parse_operands_rz(op);
    }
  else if (inst.op->format & F_RRX)
    {
      return sx_parse_operands_rrx(op);
    }
  else if (inst.op->format & F_RV)
    {
      return sx_parse_operands_rr_rv_general(op);
    }
  else 
    {
      BUG("Unidentified assembly instruction format: %d",
	  inst.op->format);
      return op;
    }
}

/* checks if flags are allowed for a given instruction (the information
   about this is stored in the "flag" member of the "sx_opcode" struct). */
static bfd_boolean
sx_check_instruction_flags(int flags)
{
  /* check for each of the flags that is set in the "flags" variable if it
     is allowed to be used with a given instruction (the information about
     this is present in the "sx_opcode.flag" variable) */
  if ((flags & FS_SPOP) && ((inst.op->flag & FS_SPOP) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_MSKOP) && ((inst.op->flag & FS_MSKOP) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_SELVU) && ((inst.op->flag & FS_SELVU) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_SELEL) && ((inst.op->flag & FS_SELEL) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_ZEROH) && ((inst.op->flag & FS_ZEROH) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_EXTOP) && ((inst.op->flag & FS_EXTOP) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_VOVER) && ((inst.op->flag & FS_VOVER) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_BPYES) && ((inst.op->flag & FS_BPYES) == 0))
    {
      return FALSE;
    }
  if ((flags & FS_BPNO) && ((inst.op->flag & FS_BPNO) == 0))
    {
      return FALSE;
    }
  
  return TRUE;
}
 

/***
 * mnemonic + flags parsing
 */
static char *
sx_parse_op (char *op, struct sx_insn *ins)
{
  char opcs[40], *p = op, *op_start;
  char *oper_start ATTRIBUTE_UNUSED;
  int i;
  int insn_flags = 0;
  struct sx_opcode *format;
  
  while (ISSPACE(*p)) p++;
  
  op_start = p;
  
  for (i = 0;
       !ISSPACE(*p) && !SX_ISSPECIAL(*p) && *p != END_OF_INSN && i < 40; 
       i++, p++)
    opcs[i] = *p;
    
  opcs[i] = '\0';
  
  if ((format = hash_find (op_hash, opcs)) == NULL)
    {
      sx_error("Invalid mnemonic '%'.\n", opcs);
      return NULL;
    }

  /* tok->opcode = format->opcode; */
  ins->op = format;

  for(i = 0; SX_ISSPECIAL(*p) && i < 4; i++, p++) {
    int flag = SX_SPECIAL_FLAG(*p);
    if (!sx_check_instruction_flags(flag))
      {
	sx_error("Suffix '%c' is not allowed for instruction '%s'.\n",
		 *p, opcs);
	return NULL;
      }
    insn_flags |= SX_SPECIAL_FLAG(*p);
  }

  ins->flags = insn_flags;

  while (ISSPACE(*p)) p++;

  /* at this point we should have the operands start at p */
  return p;
}

/*****************************************************************************/
/* Pseudo-op handlers                                                        */
/*****************************************************************************/

static void
sx_pseudo_unimpl (int nbytes ATTRIBUTE_UNUSED)
{
  as_warn("Unimplemented pseudo-op used.");
  ignore_rest_of_line();
}

/* TODO: support bits:expression parameters */
static void
sx_cons (int nbytes)
{
  expressionS exp;

  /* SX assembler expects n-byte constant to be aligned on an n-byte boundary */
  if(sx_align_bytes(nbytes) < 0)
    {
      BUG("Only constants with size of a power of two are allowed.");
      ignore_rest_of_line();
      return;
    }

  if (is_it_end_of_statement())
    {
      demand_empty_rest_of_line();
      current_ptr = frag_more(nbytes);
      memset(current_ptr, 0x00, nbytes);
      return;
    }
  
  do
    {
      current_ptr = frag_more(nbytes);
      expression_and_evaluate(&exp);
      if(exp.X_op == O_illegal)
	{
	  sx_error("Illegal constant expression.");
	  ignore_rest_of_line();
	  return;
	}
      else if(exp.X_op == O_constant) 
	{
	  md_number_to_chars(current_ptr, exp.X_add_number, nbytes);
	}
      else
	{
	  if(8 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_RELLLNG);
	  else if(4 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_RELLONG);
	  else if(1 == nbytes || 2 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_ABS);
	  else
	    {
	      ignore_rest_of_line();
	      sx_error("Unknown constant size "
		       "(only 1, 2, 4 or 8 bytes allowed).\n");
	      return;
	    }
	}
    }
  while(*input_line_pointer++ == ',');
  
  input_line_pointer--;
  demand_empty_rest_of_line ();
}

static void
sx_repeat_cons (int nbytes)
{
  int c = 0;
  char *rstart;
  
  int num_repeats;
  int i;
  expressionS exp;
  
  /* SX assembler expects n-byte constant to be aligned on an n-byte boundary */
  if(sx_align_bytes(nbytes) < 0)
    {
      BUG("Only constants with size of a power of two are allowed.");
      ignore_rest_of_line();
      return;
    }
 
  expression (&exp);
  if (exp.X_op == O_constant)
    {
      num_repeats = (int)exp.X_add_number;
    }
  else
    {
      sx_error("Number of repeats must be a constant.");
      ignore_rest_of_line();
      return;
    }
  
  c = *input_line_pointer;
  rstart = input_line_pointer;

  for (i = 0; i < num_repeats; i++) {
    input_line_pointer = rstart;
    while ((c = *input_line_pointer) == ',') {
      ++input_line_pointer;
      expression (&exp);
      current_ptr = frag_more (nbytes);

      if(exp.X_op == O_illegal)
	{
	  sx_error("Illegal constant expression.");
	  ignore_rest_of_line();
	  return;
	}
      else if(exp.X_op == O_constant) 
	{
	  md_number_to_chars(current_ptr, exp.X_add_number, nbytes);
	}
      else
	{
	  if(8 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_RELLLNG);
	  else if(4 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_RELLONG);
	  else if(1 == nbytes || 2 == nbytes)
	    fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, nbytes,
			&exp, FALSE, BFD_RELOC_SX_ABS);
	  else
	    sx_error("Unknown constant size "
		     "(only 1, 2, 4 or 8 bytes allowed).\n");
	}

      dwarf2_emit_insn (nbytes);
    }
  }
  
  demand_empty_rest_of_line ();
}

static void
sx_str (int null_term)
{
  stringer(SX_BITS_PER_CHAR | (null_term?1:0));
}

static void
sx_repeat_str (int null_term)
{
  expressionS exp;
  char *op, *ss, *se;
  int num_repeats, sl, i;

  expression (&exp);
  if (exp.X_op == O_constant)
    {
      num_repeats = (int)exp.X_add_number;
    }
  else
    {
      sx_error("Number of repeats must be a constant.");
      ignore_rest_of_line();
      return;
    }

  if(NULL == (op = sx_parse_comma(input_line_pointer)))
    {
      sx_error("Missing string constant.");
      ignore_rest_of_line();
      return;
    }
  input_line_pointer = op;
  input_line_pointer = sx_skip_whitespace(input_line_pointer);
  if(*input_line_pointer != '\"')
    {
      sx_error("Missing string.");
      ignore_rest_of_line();
      return;
    }
  ss = ++input_line_pointer;
  while(*input_line_pointer != '\"' && !is_it_end_of_statement())
    input_line_pointer++;
  if(*input_line_pointer != '\"')
    {
      sx_error("Unterminated string constant.");
      ignore_rest_of_line();
      return;
    }
  se = input_line_pointer;
  input_line_pointer++;
  sl = se - ss;
  if(sl > 0)
    {
      for (i = 0; i < num_repeats; i++)
	{
	  current_ptr = frag_more(sl + (null_term?1:0));
	  memcpy(current_ptr, ss, sl);
	  if(null_term)
	    *(current_ptr + sl) = 0;
	}
    }

  demand_empty_rest_of_line();
}

/* parse an IEEE floating point number which is represented as a hexadecimal 
   string and generate code for it. */
static char*
sx_float_ieee_hex(char *line_ptr, unsigned int nbytes, LITTLENUM_TYPE *words)
{
  char *tmp_ptr;
  unsigned int i;
  long int hex_num;
  char buff[5];
  
  know((nbytes % 2) == 0);
  
  /* extract hexadecimal number. */
  tmp_ptr = sx_extract_hex_num(line_ptr);
  if (tmp_ptr == NULL)
    {
      sx_error("Invalid IEEE number in hex form.");
      return NULL;
    }
  END_STRING_AND_SAVE(tmp_ptr);
  
  /* skip the "0x" prefix. */
  line_ptr += 2;
  
  /* check if the hexadecimal string is of correct length. the length has to be
     twice as long as the number of bytes the floating point number 
     occupies. */
  if (strlen(line_ptr) != (nbytes * 2))
    {
      sx_error("Incorrect length of hex string.");
      RESTORE_END_STRING(tmp_ptr);
      return NULL;
    }
  
  /* add the null terminator to finish the buffer. */
  buff[4] = '\0';
  for (i=0; i<nbytes/2; i++)
    {
      /* convert hexadecimal string to words, 2 chars at a time. */
      buff[0] = *line_ptr;
      buff[1] = *(line_ptr + 1);
      buff[2] = *(line_ptr + 2);
      buff[3] = *(line_ptr + 3);
        
      hex_num = strtol(buff, NULL, 16); 
      words[i] = hex_num;
      
      line_ptr += 4;
    }
  
  RESTORE_END_STRING(tmp_ptr);
  
  return tmp_ptr;
}

/* parse an IEEE floating point number and generate code for it. */
static char *
sx_float_ieee(char *line_ptr, unsigned int nbytes)
{
  LITTLENUM_TYPE words[MAX_LITTLENUMS];
  int num_words;
  int i;

  know((nbytes % 2) == 0);
  num_words = nbytes / 2;
  
  if (strstr(line_ptr, "0x") == line_ptr || strstr(line_ptr, "0X") == line_ptr)
    {
      /* the floating point number is represented by a hex string. */
      line_ptr = sx_float_ieee_hex(line_ptr, nbytes, words);
    }
  else
    {
      /* the floating point number is represented by a dot-separated
         number (i.e. the usual way). */
      switch (nbytes)
        {
        case 4:
          /* we are dealing with a single precision floating-point number
             (i.e. 32-bit) */
          line_ptr = atof_ieee (line_ptr, 'f', words);
          break;
        case 8:
          /* we are dealing with a double precision floating-point number
             (i.e. 64-bit) */
          line_ptr = atof_ieee (line_ptr, 'd', words);
          break;
        case 16:
          /* we are dealing with a quadruple precision floating-point number
             (i.e. 128-bit ) */
          line_ptr = atof_ieee(line_ptr, 'q', words);
          break;
        default:
          BUG("Unsupported IEEE floating point format: %d", nbytes);
          return NULL;
        }
    }
  
  if (line_ptr == NULL)
    {
      sx_error("The parsing of the IEEE floating point number failed.");
      return NULL;
    }

  current_ptr = frag_more(nbytes);
  for (i = 0; i < num_words; i++)
    {
      md_number_to_chars(current_ptr, (valueT) words[i], 2);
      current_ptr += 2;
    }
  
  return line_ptr;
}

static void
sx_float (int nbytes)
{
  char *line_ptr;
  int nbytes_align;
  
  /* if we encounter a floating point number and no floating-point mode is 
     was explicitly selected, choose the "float0" mode (i.e. the IEEE 
     floating point format). */
  if (sx_float_mode == FT_NON)
    {
      sx_float_mode = FT_FL0;
    }

  /* SX assembler expects n-byte floating point numbers to be aligned on an 
     n-byte boundary (if this boundary is greater than 8, an 8 is used). */
  nbytes_align = nbytes < 8 ? nbytes : 8;
  if(sx_align_bytes(nbytes_align) < 0)
    {
      BUG("Only constants with size of a power of two are allowed.");
      ignore_rest_of_line();
      return;
    }

  if (is_it_end_of_statement())
    {
      sx_error("The floating point operand is missing.");
      demand_empty_rest_of_line();
      return;
    }
  
  do
    {
      switch (sx_float_mode)
        {
        case FT_FL0:
          line_ptr = sx_float_ieee(input_line_pointer, nbytes);
          break;
        default:
          BUG("Only float0 mode is currently implemented.");
          ignore_rest_of_line();
          return;
        }
      
      if (line_ptr == NULL)
        {
          sx_error("Floating point number parsing failed.");
          ignore_rest_of_line();
          return;
        }
      
      input_line_pointer = line_ptr;
    }
  while(*input_line_pointer++ == ',');
  
  input_line_pointer--;
  demand_empty_rest_of_line ();

}

static void
sx_repeat_float (int nbytes)
{
  int i, num_repeats;
  expressionS exp;
  char *line_ptr;
  char *rstart;
  int nbytes_align;

  /* if we encounter a floating point number and no floating-point mode is 
     was explicitly selected, choose the "float0" mode (i.e. the IEEE 
     floating point format). */
  if (sx_float_mode == FT_NON)
    {
      sx_float_mode = FT_FL0;
    }

  /* SX assembler expects n-byte floating point numbers to be aligned on an 
     n-byte boundary (if this boundary is greater than 8, an 8 is used). */
  nbytes_align = nbytes < 8 ? nbytes : 8;
  if(sx_align_bytes(nbytes_align) < 0)
    {
      BUG("Only constants with size of a power of two are allowed.");
      ignore_rest_of_line();
      return;
    }

  if (is_it_end_of_statement())
    {
      sx_error("The floating point operand is missing.");
      demand_empty_rest_of_line();
      return;
    }

  expression (&exp);
  if (exp.X_op == O_constant)
    {
      num_repeats = (int)exp.X_add_number;
    }
  else
    {
      sx_error("Number of constant repeats must be a constant.");
      return;
    }

  rstart = input_line_pointer;
  for (i = 0; i < num_repeats; i++)
    {
      input_line_pointer = rstart;
      while (*input_line_pointer == ',') 
        {
          /* skip ','. */
          ++input_line_pointer;
          
          switch (sx_float_mode)
            {
            case FT_FL0:
              line_ptr = sx_float_ieee(input_line_pointer, nbytes);
              break;
            default:
              BUG("Only float0 mode is currently implemented.");
              ignore_rest_of_line();
              return;
            }          
          if (line_ptr == NULL)
            {
              sx_error("Floating point number parsing failed.");
              ignore_rest_of_line();
              return;
            }
          
          input_line_pointer = line_ptr;                  
        }
    }
  
  demand_empty_rest_of_line ();  
}

static void
sx_address (int nbytes ATTRIBUTE_UNUSED)
{
  expressionS exp;

  /* adcn defines an entry address that conforms to the procedure call
     convention for SUPER-UX linkage. */

  /* SX assembler expects address to be aligned on an 8-byte boundary */
  sx_align_bytes(8);

  /* expression must be a relocatable value (i.e. a symbol) from a text
     section */

  do
    {
      current_ptr = frag_more(8);
      expression(&exp);
      if(exp.X_op == O_symbol)
	{
	  fix_new_exp(frag_now, current_ptr - frag_now->fr_literal, 8,
		      &exp, FALSE, BFD_RELOC_SX_RELPROC);
	}
      else
	{
	  sx_error("An address of a symbol in text section must be used "
		   "in 'adcn' pseudo-op.");
	}
    }
  while(*input_line_pointer++ == ',');

  input_line_pointer--;
  demand_empty_rest_of_line();
}

static void
sx_lcomm (int system ATTRIBUTE_UNUSED)
{
  /* create a .[s]lcomm section */

  int idx = system?SX_SEC_IDX_SLCOMM:SX_SEC_IDX_LCOMM;

  sx_set_section(idx);

  demand_empty_rest_of_line ();
}

static void
sx_lglobal (int system ATTRIBUTE_UNUSED)
{
  /* TODO */
  as_warn("[s]lglobal pseudo-op not implemented yet.");

  input_line_pointer = find_end_of_line(input_line_pointer, 0);
  demand_empty_rest_of_line ();
}

static void
sx_lstat (int system ATTRIBUTE_UNUSED)
{
  /* TODO */
  as_warn("[s]lstat pseudo-op not implemented yet.");

  input_line_pointer = find_end_of_line(input_line_pointer, 0);
  demand_empty_rest_of_line ();
}

static void
sx_taskcomm (int system ATTRIBUTE_UNUSED)
{
  /* TODO */
  as_warn("[s]taskcomm pseudo-op not implemented yet.");

  input_line_pointer = find_end_of_line(input_line_pointer, 0);
  demand_empty_rest_of_line ();
}

static void
sx_using (int a ATTRIBUTE_UNUSED)
{
  sx_reg_entry reg;
  expressionS exp;
  int c; 
  char *endp;

  expression (&exp);
  if(exp.X_op != O_symbol)
    {
      sx_error("No symbol label in 'using' pseudo-op.");
      return;
    }
  
  c = (int)(*input_line_pointer);
  
  if (c != ',') 
    {
      sx_error("No base register in 'using' pseudo-op.");
      return;
    }
  ++input_line_pointer;
  
  /* Following expression must be a register */
  endp = sx_parse_scalar_reg(input_line_pointer, &reg);
  if (endp == NULL) 
    {
      sx_error("No base register in 'using' pseudo-op");
      return;
    }
  input_line_pointer = endp;

  using_set = 1;
  using_base_reg = reg;
  /* we store the whole label expression here, as symbols need not (actually,
     they usually don't) resolve at this point. we will be doing fixups later
     on anyway */
  using_base_val = exp;

  demand_empty_rest_of_line();
}

static void
sx_drop (int a ATTRIBUTE_UNUSED)
{
  sx_reg_entry reg;
  char *endp;

  /* following expression must be a register */
  endp = sx_parse_scalar_reg(input_line_pointer, &reg);
  if (endp == NULL) 
    {
      sx_error("No base register was expected in 'using' pseudo-op.");
      return;
    }
  input_line_pointer = endp;

  if(reg.num != using_base_reg.num)
    {
      sx_error("You need to drop the register you were using, "
	       "not another one ...");
      return;
    }

  using_set = 0;
}

static void
sx_even (int a ATTRIBUTE_UNUSED)
{
  sx_align_bytes(2);
}

static void
sx_bss (int a ATTRIBUTE_UNUSED)
{
  char *name, *p, *tmp_op;
  char c;
  valueT size, align;
  symbolS *sym;
  expressionS exp;
  int bit_align;

  /* symbol name */
  name = input_line_pointer;
  c = get_symbol_end();
  p = input_line_pointer;
  *p = c;

  if(name == p)
    {
      sx_error("Expecting symbol name as the first parameter to bss "
	       "pseudo-op.");
      ignore_rest_of_line();
      return;
    }

  tmp_op = sx_parse_comma(input_line_pointer);
  if (tmp_op == NULL) 
    {
      sx_error("Expecting symbol size as the second parameter to bss "
	       "pseudo-op");
      ignore_rest_of_line();
      return;
    }
  input_line_pointer = tmp_op;

  /* size expression */
  expression_and_evaluate(&exp);
  if(exp.X_op != O_constant)
    {
      sx_error("Expecting symbol size as the second parameter to bss "
	       "pseudo-op");
      ignore_rest_of_line();
      return;
    }
  size = exp.X_add_number;

  /* handle optional alignment */
  if(is_it_end_of_statement())
    {
      align = 1;
    }
  else
    {
      tmp_op = sx_parse_comma(input_line_pointer);
      if (tmp_op == NULL) 
	{
	  sx_error("Expecting alignment as the third parameter to bss "
		   "pseudo-op");
	  ignore_rest_of_line();
	  return;
	}
      input_line_pointer = tmp_op;
      /* optional alignment expression */
      expression_and_evaluate(&exp);
      if(exp.X_op != O_constant)
	{
	  sx_error("Expecting alignment as the third parameter "
		   "to bss pseudo-op");
	  ignore_rest_of_line();
	  return;
	}
      align = exp.X_add_number;
    }

  bit_align = val_to_pow2(align);
  if(bit_align < 0)
    {
      sx_error("Alignment must be a power of two.");
      ignore_rest_of_line();
      return;
    }

  *p = 0;
  sym = symbol_find_or_make(name);
  if(S_IS_DEFINED(sym))
    {
      sx_error("Attempting to re-define symbol 'name'.");
      ignore_rest_of_line();
      return;
    }
  bss_alloc(sym, size, bit_align);

  demand_empty_rest_of_line ();  
}

static void
sx_ident (int n ATTRIBUTE_UNUSED)
{
  /* remember current section */
  subsegT old_subseg = now_subseg;
  segT old_seg = now_seg;

  /* change to (and create if needed) a .comment section */
  sx_set_section(SX_SEC_IDX_COMMENT);

  stringer(SX_BITS_PER_CHAR | 1);

  input_line_pointer--;
  demand_empty_rest_of_line ();

  /* return to old section */
  subseg_set(old_seg, old_subseg);
}

#ifdef SX_SUPPORT_WEAK_SYMBOLS
static void
sx_weak (int n ATTRIBUTE_UNUSED)
{
  char *name, *p, *tmp_op;
  char c;
  symbolS *sym;

  do {
    /* symbol name */
    name = input_line_pointer;
    c = get_symbol_end();
    p = input_line_pointer;
    *p = c;

    if(name == p)
      {
	sx_error("Expecting symbol name as the first parameter to bss "
		 "pseudo-op.");
	ignore_rest_of_line();
	return;
      }

    GAS_DBG_PRINT("Weak sym: %s\n", name);
    *p = 0;
    sym = symbol_find_or_make(name);
    S_SET_WEAK(sym);

    tmp_op = sx_parse_comma(input_line_pointer);
    if (tmp_op == NULL) 
      {
	demand_empty_rest_of_line();
	break;
      }
    input_line_pointer = tmp_op;
  } while(1);

}

static void
sx_wcomm (int n ATTRIBUTE_UNUSED)
{
  /* TODO */
  as_warn("wcomm pseudo-op not implemented yet.");

  input_line_pointer = find_end_of_line(input_line_pointer, 0);
  demand_empty_rest_of_line ();
}

#endif /* SX_SUPPORT_WEAK_SYMBOLS */


/*****************************************************************************/
/* Code generation                                                           */
/*****************************************************************************/

/* returns:
   0 - if final displacement was generated,
   1 - if fixup was created.
*/
static inline int
sx_gen_displacement(expressionS *displ, int *field)
{
  if(displ->X_op == O_constant)
    {
      /* expression is constant: generate final displacement */
      md_number_to_chars((char *)field, displ->X_add_number, 4);
      return 0;
    }
  else if(displ->X_op != O_illegal)
    {
      fixS *fix;
      fix = fix_new_exp(frag_now, current_ptr - frag_now->fr_literal + 4, 4,
			displ, FALSE, BFD_RELOC_SX_RELLONG);
      fix->fx_addnumber = FIX_DISPLACEMENT;
      return 1;
    }
  else
    {
      BUG("Illegal expression specified for displacement (%d).",
	  (int)displ->X_op);
      return -1;
    }
}


/* fills the field with the data about scalar register. */
static inline void
sx_gen_register(sx_reg_entry *reg, bfd_boolean set_tag_to_one, 
		unsigned char *field)
{
  /* assign register number to the lowest 7 bits of the field */
  *field = (reg->num & 0x7F);
  
  if (set_tag_to_one)
    {
      /* set bit 0 (i.e. the leftmost bit) to 1 to mark that the current
       * field contains the register number */
      sCHARbit(*field, 0, 1);
    }
  else
    {
      /* set bit 0 (i.e. the leftmost bit) to 0 to mark that the current
       * field contains the register number */
      sCHARbit(*field, 0, 0);
    }

}

/* fills the field with the data about immediate value (either signed or 
   unsigned). */
static inline void
sx_gen_immediate(char imm, unsigned char *field)
{
  /* assign the immediate value to the lowest 7 bits of the field */
  *field = (imm & 0x7F);
  /* set bit 0 (i.e. the leftmost bit) to 0 to mark that the current
   * field contains an immediate value */
  sCHARbit(*field, 0, 0);
}

/* fills the field with the data about mask (i.e. M). */
static inline void
sx_gen_mask(sx_imm_mask *mask, unsigned char *field)
{
  /* assign the "m" value to the lowest 6 bits of the field */
  *field = (mask->m & 0x3F);
  
  /* set the bit 1 (i.e. the second leftmost bit) to the proper value based
     on the "i" parameter of the mask. */
  if (mask->i == 0)
    {
      sCHARbit(*field, 1, 1);
    }
  else if (mask->i == 1)
    {
      sCHARbit(*field, 1, 0);
    }
    
  /* set bit 0 (i.e. the leftmost bit) to 0 to mark that the current
   * field contains the mask. */
  sCHARbit(*field, 0, 0);
}



/* fills the field with the data about address syllable. This function handles 
   ASX (i.e. index_enabled=TRUE), as well as AS (i.e. index_enabled=FALSE) 
   address syllables. */
static inline void
sx_gen_address_syllable(sx_asx_entry *asx, bfd_boolean index_enabled, 
			sx_instr *inst_code)
{
  /* store data into the index register only if the index specification is
     enabled (i.e. we are dealing with ASX addresses). */
  if (index_enabled)
    {
      /* put index in the Y field */
      if(asx->index.type == SX_IDX_NONE)
        {
          /* the index register doesn't exist */
          inst_code->opc.w1.y = 0;
        }
      else if(asx->index.type == SX_IDX_REG)
        {
          /* index is scalar register */          
          sx_gen_register(&(asx->index.data.scalar_reg), TRUE,
			  &(inst_code->opc.w1.y));
        }
      else if (asx->index.type == SX_IDX_IMM)
        {
          /* index is signed 7bit immedaite */
          sx_gen_immediate(asx->index.data.imm, 
			   &(inst_code->opc.w1.y));
        }
      else
        {
          BUG("Scalar register or immediate value expected as index "
	      "of address syllable.");
          return;
        }
    }

  /* put the base register in Z field */
  if(asx->base.type == -1)
    {
      /* base is 0 */
      inst_code->opc.w1.z = 0;
    }
  else if(asx->base.type == TSREG)
    {
      /* base is sreg (either set explicitly or via using pseudoop */      
      sx_gen_register(&(asx->base), TRUE, &(inst_code->opc.w1.z));
    }
  else 
    {
      BUG("Scalar register expected as base register of address syllable.");
      return;
    }

  /* finally, generate displacement. */
  sx_gen_displacement(&(asx->displ), &(inst_code->d));

}

/* fills the fields with the data about vector result (i.e. the combination
   of $vx and $vdz registers). Based on the instruction format (i.e. RRX or RV)
   we store vector data register into the Z or the D field. */
static inline void
sx_gen_vector_result(sx_vres_entry *vres, short format, 
		     sx_instr *inst_code)
{
  /* FIXME: this function supports only RRX fromat for SX-5/SX-6/SX-7/SX-8
     architectures. Should we also write a code for supporting SX-4 
     architecture? See "Chapter 6: Instruction Formats" of the 
     SX-8 "Assembly Language Reference Manual". */
  /* @FIXME: No, we do not care about SX-4! jaKa */

  int tmp_d;
  
  if (vres->type == VRES_VREG || vres->type == VRES_BOTH)
    {
      /* assign vector register number to the right-most 3 bits of the X 
         field. */
      inst_code->opc.w1.x = vres->vreg.num & 0x7;
      sCHARbit(inst_code->opc.w1.x, 1, 0);
    }
  else 
    {
      /* no vector register is given. */
      inst_code->opc.w1.x = 0;
      sCHARbit(inst_code->opc.w1.x, 1, 1);
    }
    
  if (vres->type == VRES_VDREG || vres->type == VRES_BOTH)
    {
      if ((format & F_RV) || (format & F_RR))
        {
          /* in RV format instructions, assign vector data register number 
             to the right-most 6 bits of the Z field. */
          inst_code->opc.w1.z = vres->vdreg.num & 0x3F;
          sCHARbit(inst_code->opc.w1.z, 0, 0);
        }
      else if (format & F_RRX)
        {
          /* in RRX format instructions, assign vector data register number 
             to the right-most 6 bits of the D field. */
          tmp_d = vres->vdreg.num & 0x3F;
          sINTbit(tmp_d, 24, 0);
	  md_number_to_chars((char *)&(inst_code->d), tmp_d, 4);
        }
      else
        {
          BUG("Handling of vector results isn't implemented for instruction "
	      "format %d", format);
        }
    }
  else 
    {
      /* no vector data register is given. */
      if ((format & F_RV) || (format & F_RR))
        {
          inst_code->opc.w1.z = 0;
          sCHARbit(inst_code->opc.w1.z, 0, 1);
        }
      else if (format & F_RRX)
        {
          tmp_d = 0;
          sINTbit(tmp_d, 24, 1);
          md_number_to_chars((char *)&(inst_code->d), tmp_d, 4);
        }
      else
        {
          BUG("Handling of vector results isn't implemented for instruction "
	      "format %d", format);
        }
    }
}

/* generates code for the instruction flags */
static void
sx_gen_flags(unsigned short flags, short format, unsigned char *field)
{
  /* if one of the '*', '!' suffixes was appended to the instruction, 
     the left-most bit of the X field has to be set to 1 (Cx=1), if not, the 
     left-most bit has to be set to 0 (Cx=0). 
     WARNING: here we assume that the '*', '!' flags are mutually 
     exclusive. if at som point in time this doesn't hold anymore, the
     function will have to be corrected. */  
  if ((flags & FS_MSKOP) == FS_MSKOP || (flags & FS_EXTOP) == FS_EXTOP)
    {
      sCHARbit(*field, 0, 1);
    }
    
  if ((flags & FS_SPOP) == FS_SPOP && ((format & F_RR) || (format & F_CFX)))
    {
      /* in RR instructions, the '@' suffix sets the left-most bit of the
         X field (Cx=1). */
      sCHARbit(*field, 0, 1);
    }
  else if ((flags & FS_SPOP) == FS_SPOP  && (format & F_RV))
    {
      /* in RV instructions, the the '@' suffix sets the 4th left bit of the
         X field (Cs=1). */
      sCHARbit(*field, 3, 1);
    }
  
    
  /* if the '%' suffix was appended to the instruction, the 3rd left bit of
     the X field has to be set to 1 (Cf=1), if not, it has to be set to 0 
     (Cf=0). */
  if ((flags & FS_SELVU) == FS_SELVU)
    {
      sCHARbit(*field, 2, 1);
    }

  /* if the '=' suffixe was appended to the instruction, the left-most bit
     of the X field has to be set to 1 (Cm=1), if not, it has to be set to 0 
     (Cm=0). */
  if ((flags & FS_ZEROH) == FS_ZEROH && (format & F_RX))
    {
      /* if the '=' suffixe was appended to the instruction, the left-most bit
         of the X field has to be set to 1 (Cm=1), if not, it has to be set to 
         0 (Cm=0). */
      sCHARbit(*field, 0, 1);
    }
  else if ((flags & FS_ZEROH) == FS_ZEROH)
    {
      /* if the '=' suffix was appended to the instruction, the 4th left
         bit of the X field has to be set to 1 (Cm=1), if not, it has to be set
         to 0 (Cm=0). */
      sCHARbit(*field, 3, 1);
    }
    
  /* if the '?' suffix was appended to the instruction, the 4th left bit
     of the X field has to be set to 1 (Cm=1), if not, it has to be set to 0 
     (Cm=0). */
  if ((flags & FS_SELEL) == FS_SELEL)
    {
      sCHARbit(*field, 3, 1);
    }

  /* if the '>' suffix was appended to the instruction, the bits 2 and 3
     of the X field have to be set to 1 (BPF=11). */    
  if ((flags & FS_BPYES) == FS_BPYES)
    {
      sCHARbit(*field, 2, 1);
      sCHARbit(*field, 3, 1);
    }
  /* if the '>' suffix was appended to the instruction, the bits 2
     of the X field has to be set to 1 and the bit 3 of the X field has to be 
     set to 0 (BPF=10). */    
  if ((flags & FS_BPNO) == FS_BPNO)
    {
      sCHARbit(*field, 2, 1);
      sCHARbit(*field, 3, 0);
    }
}

/***
 * code generation for specific instruction types
 */

static int
sx_gen_code_rx(void)
{
  /* store the scalar register into the X field. */
  sx_gen_register(&(inst.ops[0].op.reg), FALSE, &(code.opc.w1.x));

  /* store address syllable (index specification disabled) into the Y, Z and 
     the D fields. */
  sx_gen_address_syllable(&(inst.ops[1].op.asx), TRUE, &code);

  return 0;
}

static int
sx_gen_code_cfx(void)
{
  int opno = 0;

  /* check param count */
  if(inst.opcount != 3)
    {
      if(((0 != strcmp(inst.op->name, "b")) &&
	  (0 != strcmp(inst.op->name, "bs")) &&
	  (0 != strcmp(inst.op->name, "bf"))) ||
	 (inst.opcount != 2))
	{
	  sx_error("CFX type instructions (%s) expect 3 arguments. "
		 "First argument is implicit except for general "
		 "branch instructions (bc, bcs, bcf).",
		 inst.op->name);
	  return -1;
	}
    }

  /* condition flags - result from parsing */
  if(inst.ops[opno].type != OP_IMM)
    {
      sx_error("No conditional flag given or implied.");
      return -1;
    }
  code.opc.w1.x |= inst.ops[opno].op.imm & 0x0F;
  opno++;

  if(inst.opcount == 3)
    {
      /* store the scalar register or immediate value into the Y field. */
      if (inst.ops[opno].type == OP_REG)
	{
	  sx_gen_register(&(inst.ops[opno].op.reg), TRUE, &(code.opc.w1.y));
	}
      else if (inst.ops[opno].type == OP_IMM)
	{
	  sx_gen_immediate(inst.ops[opno].op.imm, &(code.opc.w1.y));
	}
      else 
	{
	  BUG("Unexpected operand type.");
	  return -1;
	}
      opno++;
    }

  /* store address syllable (index specification disabled) into the Z and the D
     fields. */
  sx_gen_address_syllable(&(inst.ops[opno].op.asx), FALSE, &code);
  
  return 0;
}

/* code generation for RW-type instructions.
 
   NOTE: this function assumes that the parsing functions (i.e. the "sx_parse*"
   functions) made sure that the insturctions are valid (i.e. contain the right
   number of operands, etc.). If this assumption doesn't hold, we can 
   experience serious crashess here. */
static int
sx_gen_code_rw(void)
{

  /* store the scalar register into the X field. */
  sx_gen_register(&(inst.ops[0].op.reg), FALSE, &code.opc.w1.x);
  
  /* store the scalar register or immediate value into the Y field. */
  if (inst.ops[1].type == OP_REG)
    {
      sx_gen_register(&(inst.ops[1].op.reg), TRUE, &code.opc.w1.y);
    }
  else if (inst.ops[1].type == OP_IMM)
    {
      sx_gen_immediate(inst.ops[1].op.imm, &code.opc.w1.y);
    }
  else 
    {
      BUG("Unexpected operand type.");
      return -1;
    }
  
  /* store the scalar register or mask into the Z field. */
  if (inst.ops[2].type == OP_REG)
    {
      sx_gen_register(&(inst.ops[2].op.reg), TRUE, &code.opc.w1.z);
    }
  else if (inst.ops[2].type == OP_MASK)
    {
      sx_gen_mask(&(inst.ops[2].op.mask), &code.opc.w1.z);
    }
  else 
    {
      BUG("Unexpected operand type.");
      return -1;
    }

  return 0;
}

/* code generation for RZ-type instructions.
 
   NOTE: this function assumes that the parsing functions (i.e. the "sx_parse*"
   functions) made sure that the insturctions are valid (i.e. contain the right
   number of operands, etc.). If this assumption doesn't hold, we can 
   experience serious crashess here. */
static int
sx_gen_code_rz(void)
{
  /* store the scalar register into the X field. */
  sx_gen_register(&(inst.ops[0].op.reg), FALSE, &code.opc.w1.x);

  /* store the immediate value into the Y field. */
  sx_gen_immediate(inst.ops[1].op.imm, &code.opc.w1.y);
  
  /* store address syllable (index specification disabled) into the Z and the D
     fields. */
  sx_gen_address_syllable(&(inst.ops[2].op.asx), FALSE, &code);
  
  return 0;  
}

/* code generation for RRX-type instructions.
 
   NOTE: this function assumes that the parsing functions (i.e. the "sx_parse*"
   functions) made sure that the insturctions are valid (i.e. contain the right
   number of operands, etc.). If this assumption doesn't hold, we can 
   experience serious crashess here. */
static int
sx_gen_code_rrx(void)
{
  int op_num = 0;

  /* store the vector result into the X and D fields. */
  sx_gen_vector_result(&(inst.ops[op_num].op.vec_res_reg), inst.op->format, 
		       &code);
  
  op_num++;
    
  /* store the scalar register or immediate value into the Y field. */
  if (inst.ops[op_num].type == OP_REG)
    {
      sx_gen_register(&(inst.ops[op_num].op.reg), TRUE, &code.opc.w1.y);
    }
  else if (inst.ops[op_num].type == OP_IMM)
    {
      sx_gen_immediate(inst.ops[op_num].op.imm, &code.opc.w1.y);
    }
  else 
    {
      BUG("Unexpected operand type.");
      return -1;
    }
  op_num++;
  
  /* store the scalar register or mask into the Z field. */
  if (inst.ops[op_num].type == OP_REG)
    {
      sx_gen_register(&(inst.ops[op_num].op.reg), TRUE, &code.opc.w1.z);
    }
  else if (inst.ops[op_num].type == OP_MASK)
    {
      sx_gen_mask(&(inst.ops[op_num].op.mask), &code.opc.w1.z);
    }
  else 
    {
      BUG("Unexpected operand type.");
      return -1;
    }
    
  return 0;
}

/* map an operands format into the sequence of operand types. */
static int 
sx_map_format_to_operands_sequence(sx_opcode *inst_opc,
				   enum sx_operand_type *op_seq)
{
  int i;
  
  if (SX_OA_FMT(inst_opc->format) == FM_DXY_Z)
    {
      /* X(Y), Z */
      op_seq[0] = SX_OP_X;
      op_seq[1] = SX_OP_Y;
      op_seq[2] = SX_OP_Z;
    }
  else if (SX_OA_FMT(inst_opc->format) == FM_DX_ZY ||
    SX_OA_FMT(inst_opc->format) == FM_XZY)
    {
      /* X, Z(Y) or X, Z, Y */
      op_seq[0] = SX_OP_X;
      op_seq[1] = SX_OP_Z;
      op_seq[2] = SX_OP_Y;
    }
  else if (SX_OA_FMT(inst_opc->format) == FM_DXZ_Y)
    {
      /* (X, Z), Y */
      op_seq[0] = SX_OP_X;
      op_seq[1] = SX_OP_Y;
      op_seq[2] = SX_OP_NONE;      
    }
  else if (SX_OA_FMT(inst_opc->format) == FM_ZX)
    {
      /* Z, X */
      op_seq[0] = SX_OP_Z;
      op_seq[1] = SX_OP_X;
      op_seq[2] = SX_OP_NONE;
    }
  else if (SX_OA_FMT(inst_opc->format) == FM_XYZ)
    {
      /* X, Y, Z */
      
      /* some FM_XYZ instructions don't contain all the operands. the code
         below sets the sequence properly even in those occasions. */
      for (i=0; i<3; i++)
        {
          op_seq[i] = SX_OP_NONE;          
        }
         
      i = 0;
      if (inst_opc->field.x != 0)
        {
          op_seq[i] = SX_OP_X;
          i++;
        }
      if (inst_opc->field.y != 0)
        {
          op_seq[i] = SX_OP_Y;
          i++;
        }
      if (inst_opc->field.z != 0)
        {
          op_seq[i] = SX_OP_Z;
          i++;
        }
    }
  else {
    BUG("Unknown operand mapping for format: %hd", 
      inst_opc->format);
    return -1;
  }

  return 0;
}

/* check if a given RV instructions contains two vector registers
   as its operands (i.e. is of "V OP V" type). Check the "6.5 RV Type" chapter
   of "SX-8 CPU Functional Description Manual" for further details. */
static bfd_boolean
rv_inst_contains_two_vector_registers(sx_opcode *op)
{
  /* only RV instructions can contain two vector registers as its operands. */
  if (!(op->format & F_RV))
    return FALSE;
  
  /* intruction is of "V OP V" type if it contains two fixed vector registers
     in Y and Z field. */
  return (op->vreg.y != 0) && (op->vreg.z != 0);
}

/* checks if the imeediate value is actually a flag. Flags are not stored in 
   the instruction field as a whole but are rather used to set the specific
   bit, usually in the X field. */
static bfd_boolean
is_immediate_value_flag(sx_opcode *op, enum sx_operand_type field_type)
{
  bfd_boolean is_flag;
  
  switch (op->opcode)
    {
    case O_VFIX:
    case O_VFIXX:
      /* the optional flag is actually contained in the Z field, bit since 
         there is the "va0" fixed register in the Y field, which doesn't 
         generate any code, the optional flag is shifted to from Z to Y. */
      is_flag = (field_type == SX_OP_Y);
      break;
    default:
      is_flag = FALSE;
    }
    
    return is_flag;
}

static inline void
sx_gen_immediate_value_flag(sx_opcode *op, char imm, unsigned char *field)
{
  switch (op->opcode)
    {
    case O_VFIX:
    case O_VFIXX:
      if (imm == 1)
        {
          sCHARbit(*field, 3, 1);
        }
      break;
    default:
      BUG("Unexpected instruction opcode.");
    }
}

/* general code generation function for RR or RV-type instructions.
 
   NOTE: this function assumes that the parsing functions (i.e. the "sx_parse*"
   functions) made sure that the instructions are valid (i.e. contain the right
   number of operands, etc.). If this assumption doesn't hold, we can 
   experience serious crashes here. */
static int
sx_gen_code_rr_rv_general(void)
{
  int op_num;
  enum sx_operand_type op_seq[3];
  bfd_boolean rv_type_v_op_v;
  
  /* first, initialize all the operand fields to 0. */
  code.opc.w1.x = 0;
  code.opc.w1.y = 0;
  code.opc.w1.z = 0;
  code.d = 0;
  
  if (sx_map_format_to_operands_sequence(inst.op, op_seq) != 0)
    {
      BUG("Unable to map instruction format to operands sequence.");
    }
  
  rv_type_v_op_v = TRUE;
  for (op_num=0; op_num<inst.opcount; op_num++)
    {
      switch (op_seq[op_num])
        {
        case SX_OP_X:
          
          /* store the register (scalar, vector, vector mask) or 
             vector result into the X field. */
          if (inst.ops[op_num].type == OP_REG && 
	      inst.ops[op_num].op.reg.type == TSREG)
            {
              /* when the register is scalar register the left-most bit of X  
                 field has to be 1. */
              sx_gen_register(&(inst.ops[op_num].op.reg), FALSE,
			      &code.opc.w1.x);
            }
          else if (inst.ops[op_num].type == OP_REG && 
		   (inst.ops[op_num].op.reg.type == TVREG ||
		    inst.ops[op_num].op.reg.type == TVMREG))
            {
              /* when the register is vector (mask) register the left-most bit  
                 of X field has to be 0. */
              sx_gen_register(&(inst.ops[op_num].op.reg), FALSE,
			      &code.opc.w1.x);
            }
          else if (inst.ops[op_num].type == OP_VRES)
            {
              sx_gen_vector_result(&(inst.ops[op_num].op.vec_res_reg), 
				   inst.op->format, &code);
            }
          else if (inst.ops[op_num].type == OP_IMM)
            {
              if (is_immediate_value_flag(inst.op, op_seq[op_num]))
                {
                  /* dont't store the whole immediate value but rather set a
                     specific bit in the X field. */
                  sx_gen_immediate_value_flag(inst.op, inst.ops[op_num].op.imm, 
                    &code.opc.w1.x);
                }
              else
                {
                  /* this is ordinary immediate value. store it into the 
                     field. */
                  sx_gen_immediate(inst.ops[op_num].op.imm, &code.opc.w1.x);
                }
            }
          else 
            {
              BUG("Unexpected operand type.");
              return -1;
            }
            
          break;
        case SX_OP_Y:
          /* store the register (scalar, vector mask) or immediate value into 
             the Y field. */
          if (inst.ops[op_num].type == OP_REG && 
	      inst.ops[op_num].op.reg.type == TSREG)
            {
              /* when the register is scalar register the left-most bit of Y  
                 field has to be 1. */
              sx_gen_register(&inst.ops[op_num].op.reg, TRUE, &code.opc.w1.y);
            }
          else if (inst.ops[op_num].type == OP_REG && 
		   (inst.ops[op_num].op.reg.type == TVREG ||
		    inst.ops[op_num].op.reg.type == TVMREG))
            {
              /* when the register is vector (mask) register the    
                 left-most bit of X field has to be 0. */
              sx_gen_register(&(inst.ops[op_num].op.reg), FALSE,
			      &code.opc.w1.y);
            }
          else if (inst.ops[op_num].type == OP_IMM)
            {
              if (is_immediate_value_flag(inst.op, op_seq[op_num]))
                {
                  /* dont't store the whole immediate value but rather set a
                     specific bit in the X field. */
                  sx_gen_immediate_value_flag(inst.op, inst.ops[op_num].op.imm, 
                    &code.opc.w1.x);
                }
              else
                {
                  /* this is ordinary immediate value. store it into the 
                     field. */
                  sx_gen_immediate(inst.ops[op_num].op.imm, &code.opc.w1.y);
                }
            }
          else if (inst.ops[op_num].type == OP_MASK)
            {
              sx_gen_mask(&inst.ops[op_num].op.mask, &code.opc.w1.y);
            }            
          else 
            {
              BUG("Unexpected operand type.");
              return -1;
            }

          /* mark that the instruction contains something other than a vector
             register in the Y field (this is relevant only for RV 
             registers). */
          rv_type_v_op_v = FALSE;

          break;
        case SX_OP_Z:
          /* store the register (scalar, vector, vector data) or mask (
             sometimes even an immediate value) into the Z field. */
          if (inst.ops[op_num].type == OP_REG && 
	      inst.ops[op_num].op.reg.type == TSREG)
            {
              /* when the register is scalar register the left-most bit of Z  
                 field has to be 1. */
              sx_gen_register(&(inst.ops[op_num].op.reg), TRUE, &code.opc.w1.z);
            }
          else if (inst.ops[op_num].type == OP_REG && 
		   (inst.ops[op_num].op.reg.type == TVREG || 
		    inst.ops[op_num].op.reg.type == TVDREG ||
		    inst.ops[op_num].op.reg.type == TVMREG))
            {
              /* when the register is vector (data, mask) register the    
                 left-most bit of Z field has to be 0. */
              sx_gen_register(&(inst.ops[op_num].op.reg), FALSE,
			      &code.opc.w1.z);
            }
          else if (inst.ops[op_num].type == OP_MASK)
            {
              sx_gen_mask(&(inst.ops[op_num].op.mask), &code.opc.w1.z);
            }
          else if (inst.ops[op_num].type == OP_IMM)
            {
              if (is_immediate_value_flag(inst.op, op_seq[op_num]))
                {
                  /* dont't store the whole immediate value but rather set a
                     specific bit in the X field. */
                  sx_gen_immediate_value_flag(inst.op, inst.ops[op_num].op.imm, 
                    &code.opc.w1.x);
                }
              else
                {
                  /* this is ordinary immediate value. store it into the 
                     field. */
                  sx_gen_immediate(inst.ops[op_num].op.imm, &code.opc.w1.z);
                }
            }          
          else 
            {
              BUG("Unexpected operand type.");
              return -1;
            }

          break;
        case SX_OP_NONE:
          /* do nothing. */
          break;
        }        
    }  
    
    if (rv_inst_contains_two_vector_registers(inst.op) && rv_type_v_op_v)
      {
        /* if RV instruction contains two fixed vector registers as its  
           operands the Y field contains all ones....
           
           ...well, not exactly. There seem to be some instructions which
           have two fixed vector registers and don't contain ones in the
           Y field (one of such functions is "vfsm"). */
        if (inst.op->opcode != O_VFSM)
          {        
            code.opc.w1.y = 0xff;
          }
      }
    
  return 0;  
}


/*
 * Generate code out of the parsed instruction
 *
 * Precondition:  inst contains complete information about
 *                the parsed instruction to generate machine
 *                code from
 * Postcondition: code contains the generated machine code
 *
 * Returns: number of bytes generated if OK, <= 0 if error
 */

static int
sx_gen_code(void)
{
  int words = 0, i;
  short format = inst.op->format;

  code.opc.w2 = 0;
  code.d = 0;

  /* are we working on a 2 word instruction? */
  if (format & FMASK_8BYTE)
    words = 2*4;
  else
    words = 4;

  current_ptr = frag_more(words);

  GAS_DBG_PRINT("Generating code.\n");
  GAS_DBG_PRINT("Instruction %s, flags 0x%x, %d operands.\n",
		inst.op->name, inst.flags, inst.opcount);
  for(i = 0; i < inst.opcount; i++) 
    {
      GAS_DBG_PRINT("Op %d: type %d\n", i, inst.ops[i].type);
      if(inst.ops[i].type == OP_IMM)
	{
	  GAS_DBG_PRINT("Immediate 0x%x (%d)\n",
			inst.ops[i].op.imm, inst.ops[i].op.imm);
	}
      else if(inst.ops[i].type == OP_REG)
	{
	  GAS_DBG_PRINT("Register %d, type %d\n",
			inst.ops[i].op.reg.num,
			inst.ops[i].op.reg.type);
	}
       else if(inst.ops[i].type == OP_MASK)
	{
	  GAS_DBG_PRINT("Mask (%d)%d\n",
			inst.ops[i].op.mask.m,
			inst.ops[i].op.mask.i);
	}
        else if(inst.ops[i].type == OP_ASX)
	{
	  GAS_DBG_PRINT("ASX D = 0x%08llx\n",
			inst.ops[i].op.asx.displ.X_add_number);
	}
    }

  /* write opcode */
  code.opc.w1.op = inst.op->opcode;

  /* and now for something completely different: the operands! */
  if(format & F_RX)
    {
      if(0 != sx_gen_code_rx())
	{
	  return -1;
	}
    }
  else if(format & F_CFX)
    {
      if(0 != sx_gen_code_cfx())
	{
	  return -1;
	}
    }
  else if (format & F_RW)
    {
      if(0 != sx_gen_code_rw())
        {
          return -1;
        }
    }
  else if (format & F_RZ)
    {
      if(0 != sx_gen_code_rz())
        {
          return -1;
        }
    }
  else if (format & F_RRX)
    {
      if (0 != sx_gen_code_rrx())
        {
          return -1;
        }
    }
  else if (format & F_RR)
    {
      if (0 != sx_gen_code_rr_rv_general())
        {
          return -1;
        }
    }
  else if (format & F_RV)
    {
      if (0 != sx_gen_code_rr_rv_general())
        {
          return -1;
        }
    }
  else
    {
      BUG("Unidentified assembly instruction format: %d",
	  inst.op->format);
    }

  /* generate the code for the instruction flags. */
  sx_gen_flags(inst.flags, inst.op->format, &(code.opc.w1.x));

  GAS_DBG_PRINT("Code: 0x%02x 0x%02x 0x%02x 0x%02x 0x%08x\n\n",
		code.opc.w1.op, code.opc.w1.x, code.opc.w1.y,
		code.opc.w1.z, code.d);

  /* The global structure 'code' contains now all the necessary
     information (it is actually a one-to-one mapping to the
     fragment.) */
  memcpy((void *)current_ptr, (void *)&code, words);

  return words;
}


/* Generate instructions */
static void
output_insn (void)
{
  fragS *insn_start_frag;
  offsetT insn_start_off;
  int fraglen;             /* We have at least 32bit instructions */

  /* Tie dwarf2 debug info to the address at the start of the insn.
     We can't do this after the insn has been output as the current
     frag may have been closed off.  eg. by frag_var.  */
  dwarf2_emit_insn (0);

  insn_start_frag = frag_now;
  insn_start_off = frag_now_fix ();

  if ((fraglen = sx_gen_code()) <= 0)
    {
      sx_error("Error while generating code.");
      return;
    }
 
  dwarf2_emit_insn (fraglen);
}


/*****************************************************************************/
/* GAS entry points                                                          */
/*****************************************************************************/

void
md_begin(void)
{
  const char *retval = NULL;
  int i;
  
  save_stack_p = save_stack;

  op_hash = hash_new ();
  
  for (i = 0; i < sx_num_opcodes; i++) {
    retval = hash_insert (op_hash, sx_opcodes[i].name, (PTR)&sx_opcodes[i]);
    if (retval != NULL && strcmp (retval, "exists") != 0)
      as_fatal (_("Can't hash instruction '%s':%s"),
                sx_opcodes[i].name, retval);
  }

  bfd_set_arch_mach(stdoutput, bfd_arch_sx, sx_flavour);

  using_set = 0;
}

void
sx_end_of_source ()
{
  unsigned char flags;
  
  /* set the "f_flttype" field of COFF header. */
  coff_data(stdoutput)->f_flttype = sx_float_mode;
  
  /* set the "f_flags2" field of COFF header. */
  flags = 0;
  if (sx_f2_lp64)
    flags |= F2_LP64;
  if (sx_f2_c_lang)
    flags |= F2_CLANG;
  if (sx_f2_bit6_used)
    flags |= F2_USEB6;
  if (sx_f2_int64)
    flags |= F2_INT64;
  if (sx_f2_vector_instr)
    flags |= F2_VECINS;
  
  coff_data(stdoutput)->f_flags2 = flags;
  
  /* set the "f_flags3" field of COFF header. */
  flags = 0;
  
  if (sx_f3_size_tmix)
    flags |= F3_SIZE_TMIX;
  if (sx_f3_size_t64)
    flags |= F3_SIZE_T64;
  if (sx_f2_vector_instr && sx_f3_max_vl512_ready && sx_f3_vl512_only)
    flags |= F3_VL512ONLY;
  if (sx_f2_vector_instr && sx_f3_max_vl512_ready)
    flags |= F3_VL512MAX;
  if (sx_f3_p64)
    flags |= F3_P64;
  if (sx_f3_multi_tasking)
    flags |= F3_MLTTSK;
  
  coff_data(stdoutput)->f_flags3 = flags;
}

/*
 * Assembler driver:
 *  - call parser
 *    - fills struct sx_insn inst
 *
 *  - call output_insn
 *    - call sx_gen_code
 *      - fills struct sx_instr code
 *      - returns number of 32 bit words to be output (from code)
 *    - write machine code fragment
 */
void
md_assemble(char *op)
{
  op = sx_parse_op (op, &inst);
  
  /* start parsing the operands only if the parsing of the mnemonic was 
     successfull. */
  if (op != NULL)
    op = sx_parse_operands (op);

  if (op == NULL)
    {
      sx_error("instruction parsing failed");
    }
  else if (*op != END_OF_INSN)
    {
      sx_error("instruction parsing failed: suffix=%s", op);
    }

  /* demand_empty_rest_of_line(); */

  output_insn ();
}

char *
md_atof(int type ATTRIBUTE_UNUSED, char *litP ATTRIBUTE_UNUSED,
	int *sizeP ATTRIBUTE_UNUSED)
{
  /* TODO: implement */
  char *ret = NULL;
  
  return ret;
}

int
md_estimate_size_before_relax (fragS *fragP ATTRIBUTE_UNUSED,
                               segT segment_type ATTRIBUTE_UNUSED)
{
  BUG("Relaxation should never occur");
  return -1;
}

/* Round up a section's size to the appropriate boundary.  */
valueT
md_section_align (segT seg ATTRIBUTE_UNUSED, valueT size ATTRIBUTE_UNUSED)
{
  /* TODO: need to fix this so that nops are generated instead of null words
     in text sections */
  int align = bfd_get_section_alignment (stdoutput, seg);
  valueT mask = ((valueT) 1 << align) - 1;

  return (size + mask) & ~mask;
}

/* Where a PC relative offset is calculated from.  On the spu they
   are calculated from the beginning of the branch instruction.  */
long
md_pcrel_from (fixS *fixp ATTRIBUTE_UNUSED)
{
  /* we don't do PC-relative fix-ups, so this should *never* be called! */
  BUG("PC-relative fix-ups should never occur");  
  return 0;
}

/* apply fix-ups after assembly pass

   NOTE: currently, we do not apply any values from fix-ups if
   the symbol is weak, as this causes problems with weak aliases
   (wrong addresses are computed after relocating, leading to illegal
   instruction traps due to jumping to wrong address).
   omitting applying fix-ups at all when weak symbols are involved and
   leaving this solely to relocation steps makes the whole thing work:
   this might (probably isn't, actually) not be The Right Way(tm) to solve
   the issue, but it seems to work well for now. just remember to keep
   an eye on this!
 */
void
md_apply_fix(fixS *fixP,
             valueT *valP,
             segT seg ATTRIBUTE_UNUSED)
{
  char *dst = (char *)(fixP->fx_where + fixP->fx_frag->fr_literal);

  GAS_DBG_PRINT("\n");
  GAS_DBG_PRINT("Applying fix-up at %ld.\n", fixP->fx_where);
  GAS_DBG_PRINT("Fix value %llx\n", (unsigned long long) *valP);
  GAS_DBG_PRINT("Add symbol:\n");
  PSYM(fixP->fx_addsy);
  GAS_DBG_PRINT("Sub symbol:\n");
  PSYM(fixP->fx_subsy);
  GAS_DBG_PRINT("Offset: %llx\n", (unsigned long long) fixP->fx_offset);
  GAS_DBG_PRINT("Dot: %llx\n", (unsigned long long) fixP->fx_dot_value);

#if 0
  /* convert generic 32 and 64 bit fixups (generated by stabs directives)
     to SX reloc types */
  switch(fixP->fx_r_type)
    {
    case BFD_RELOC_32:
      fixP->fx_r_type = BFD_RELOC_SX_RELLONG;
      break;
    case BFD_RELOC_64:
      fixP->fx_r_type = BFD_RELOC_SX_RELLLNG;
      break;
    default:
      break;
    } 
#endif

  switch(fixP->fx_r_type)
    {
    case BFD_RELOC_SX_RELLONG:
      if(fixP->fx_addsy == NULL || fixP->fx_addnumber == FIX_DISPLACEMENT)
        fixP->fx_done = TRUE;
      md_number_to_chars(dst, *valP, fixP->fx_size);
      break;
    case BFD_RELOC_SX_RELLLNG:
      if(fixP->fx_addsy == NULL)
        fixP->fx_done = TRUE;
      if(fixP->fx_addsy == NULL || !S_IS_WEAK(fixP->fx_addsy))
        md_number_to_chars(dst, *valP, fixP->fx_size);
      break;
    case BFD_RELOC_SX_RELPROC:
      /* generated by adcn pseudo-op */
      /* such a fix-up:
	 - always contains a symbol, so it's never done ...
	 - can originate *only* from an adcn pseudo-op: if it contains a
	 local symbol, we must make sure it is a text section symbol. if
	 it's an external symbol, we must perform this check at link time
	 (TODO: this requires patching relocation handling).
      */
      if(fixP->fx_addsy == NULL)
        {
          sx_error("An address of a symbol in text section must be used "
                   "in 'adcn' pseudo-op.");
        }
      else if((bfd_get_section(fixP->fx_addsy->bsym) != undefined_section) &&
              (bfd_get_section(fixP->fx_addsy->bsym) != text_section))
        {
          sx_error("A symbol used in 'adcn' pseudo-op must be an "
                   "external or local text symbol.");
        }
      if(!S_IS_WEAK(fixP->fx_addsy))
        md_number_to_chars(dst, *valP, fixP->fx_size);
      break;
    case BFD_RELOC_SX_RELLCOM:
      /* generated by lcomm pseudo-op */
      /* such a fix-up always contains a symbol, so it's never done ... */
      md_number_to_chars(dst, *valP, fixP->fx_size);
      break;
    case BFD_RELOC_SX_ABS:
      /* generated for short and byte constans for which we can't
         generate a reloc: must be resolved at this time! */
      if(fixP->fx_addsy != NULL)
        sx_error("Unable to resolve an absolute fix-up. "
                 "This is probably due to a short or byte data "
                 "initialization involving an external symbol address.");
      else
        {
          md_number_to_chars(dst, *valP, fixP->fx_size);
          fixP->fx_done = TRUE;
        }
      break;
    default:
      fprintf(stderr, "Reloc generated at line %u\n",
              fixP->fx_line);
      BUG("Fix-up (relocation type %d) can not be handled.",
          fixP->fx_r_type);
      break;
    }
}

/* if a fix-up was not marked as done during applying, we need to generate
   a relocation entry for it. */
arelent *
tc_gen_reloc (asection *seg, fixS *fixp)
{
  /* woooooohooooo ... let's create a relocation! ;) */
  arelent *reloc;
  flagword sflags;

  if (! bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type))
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("Relocation %d not supported by object file format."),
		    (int) fixp->fx_r_type);
      return NULL;
    }
  
  reloc               = xmalloc (sizeof (arelent));
  reloc->sym_ptr_ptr  = xmalloc (sizeof (asymbol *));
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  reloc->address      = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->howto        = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  reloc->addend       = 0 /* fixp->fx_offset */;

  /* add reloc flag to section containing the reloc */
  sflags = bfd_get_section_flags(stdoutput, seg);
  sflags |= SEC_RELOC;
  bfd_set_section_flags(stdoutput, seg, sflags);

  return reloc;
}

/*
;;- Local variables:
;;- mode:c
;;- eval: (c-set-style "gnu")
;;- End:
*/

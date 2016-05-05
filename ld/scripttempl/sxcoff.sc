# COFF linker script for SUPER-UX on NEC SX.
#
# Based on i386coff.sc by Ian Taylor <ian@cygnus.com>.
#
# Author: jaKa Mocnik <jaka@xlab.si>
test -z "$ENTRY" && ENTRY=_start
if test -z "${DATA_ADDR}"; then
  if test "$LD_FLAG" = "N" || test "$LD_FLAG" = "n"; then
    DATA_ADDR=.
  fi
  if test "$LD_FLAG" = "r"; then
    RELOCATE="yes"
  fi
fi

# These are substituted in as variables in order to get '}' in a shell
# conditional expansion.
CTOR='.ctor : {
    *(SORT(.ctors.*))
    *(.ctor)
  }'
DTOR='.dtor : {
    *(SORT(.dtors.*))
    *(.dtor)
  }'

cat <<EOF
OUTPUT_FORMAT("${OUTPUT_FORMAT}")
${LIB_SEARCH_DIRS}

ENTRY(${ENTRY})

SECTIONS
{
  .text ${RELOCATING+ 0x400000000} : {
    PROVIDE(__sx_init_start = ABSOLUTE(.));
    *(.init)
    PROVIDE(__sx_init_end = ABSOLUTE(.));
    *(.text*)
    *(.glue_7t)
    *(.glue_7)
    *(.rdata)
    ${CONSTRUCTING+ ___CTOR_LIST__ = .; __CTOR_LIST__ = . ; 
			LONG (-1); *(.ctors); *(.ctor); LONG (0); }
    ${CONSTRUCTING+ ___DTOR_LIST__ = .; __DTOR_LIST__ = . ; 
			LONG (-1); *(.dtors); *(.dtor);  LONG (0); }
    *(.fini)
    ${RELOCATING+ etext  =  .;}
    ${RELOCATING+ _etext =  .;}
  }

  .data ${RELOCATING+ ALIGN(0x4000000)} : {
    ${RELOCATING+  __data_start__ = . ;}

    *(.data*)

    PROVIDE(__sx_lcomm_start = ABSOLUTE(.));
    *(.lcomm*)
    PROVIDE(__sx_lcomm_end = ABSOLUTE(.));

    PROVIDE(__sx_slcomm_start = ABSOLUTE(.));
    *(.slcomm*)
    PROVIDE(__sx_slcomm_end = ABSOLUTE(.));

    ${RELOCATING+*(.gcc_exc*)}

    ${RELOCATING+___EH_FRAME_BEGIN__ = . ;}
    ${RELOCATING+*(.eh_fram*)}
    ${RELOCATING+___EH_FRAME_END__ = . ;}

    ${RELOCATING+LONG(0);}
    
    ${RELOCATING+ __data_end__ = . ;}
    ${RELOCATING+ edata  =  .;}
    ${RELOCATING+ _edata  =  .;}
  }
  ${CONSTRUCTING+${RELOCATING-$CTOR}}
  ${CONSTRUCTING+${RELOCATING-$DTOR}}

  .bss ${RELOCATING+ ALIGN(0x10)} :
  { 					
    ${RELOCATING+ __bss_start__ = . ;}

    *(.bss)

    PROVIDE(__sx_common_start = ABSOLUTE(.));
    *(COMMON)
    PROVIDE(__sx_common_end = ABSOLUTE(.));

    ${RELOCATING+ __bss_end__ = . ;}
  }

  ${RELOCATING+ end = .;}
  ${RELOCATING+ _end = .;}
  ${RELOCATING+ __end__ = .;}

  .stab  0 ${RELOCATING+(NOLOAD)} : 
  {
    [ .stab ]
  }
  .stabstr  0 ${RELOCATING+(NOLOAD)} :
  {
    [ .stabstr ]
  }

  /* C entry point */
  __centry = DEFINED(main) ? ABSOLUTE(main) : 0;
  /* F77 (90?) entry point (FORTRAN PROGRAM symbol) */
  __fentry = DEFINED(MAIN__) ? ABSOLUTE(MAIN__) : 0;

  /* lcommsize == sum_{section is SLCOMM}(size(section)) */
  PROVIDE(__sx_slcomm_size = ABSOLUTE(__sx_slcomm_end - __sx_slcomm_start));
  /* ulcommsize == sum_{section is LCOMM}(size(section)) */
  PROVIDE(__sx_lcomm_size = ABSOLUTE(__sx_lcomm_end - __sx_lcomm_start));
  /* lcommf77sz */
  PROVIDE(__sx_lcommf77_size = 0);   /* TODO */
  /* ulcommf77sz */
  PROVIDE(__sx_ulcommf77_size = 0);  /* TODO */

  PROVIDE(_pthr_sys_blsize = ABSOLUTE(__sx_lcommf77_size));
  PROVIDE(_pthr_usr_blsize = ABSOLUTE(__sx_ulcommf77_size));

  /* (S)LCOMM symbols */
  PROVIDE(_pthr_sys_dlsize = ABSOLUTE(__sx_slcomm_size)); 
  PROVIDE(_pthr_usr_dlsize = ABSOLUTE(__sx_lcomm_size));
  /* lcommbegin == PADDR(SLCOMM section) */
  PROVIDE(_pthr_sys_slcomm = ABSOLUTE(__sx_slcomm_start)); 
  /* ulcommbegin == PADDR(LCOMM section) */
  PROVIDE(_pthr_usr_slcomm = ABSOLUTE(__sx_lcomm_start));

  PROVIDE(_pthr_stksz = 0);      /* thread stack */
  
  /* f77 common block start address (in bss section) */
  PROVIDE(_init_ucomm_addr = ABSOLUTE(__sx_common_start));
  /* ... and size ... */
  PROVIDE(_init_ucomm_size = ABSOLUTE(__sx_common_end - __sx_common_start));

  /* value to fill bss with on load (SX supports 0, NaN or custom hex fill; we
     currently hardcode 0) */
  PROVIDE(_init_bss_value  = ABSOLUTE(0));

  PROVIDE(ts_lsize = ABSOLUTE(__sx_slcomm_size + __sx_lcomm_size));
  PROVIDE(ts_lsizeall = ABSOLUTE(ts_lsize + __sx_lcommf77_size + __sx_ulcommf77_size));
  PROVIDE(ts_slcomm = ABSOLUTE(__sx_slcomm_start));
  PROVIDE(ts_maxtask = 1);
  PROVIDE(ts_size = 0x800000000);

  PROVIDE(_para_nest = 0);
}
EOF

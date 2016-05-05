/* CPU definition for NEC SX machines.

   Copyright 2007, 2008 NEC HPCE

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.

   Authors: Erich Focht <efocht@hpce.nec.com>
            jaKa Mocnik <jaka@xlab.si>
            Marko Novak <marko.novak@xlab.si>
            Matthias Hess
   
*/

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"


static const bfd_arch_info_type *
sx_compatible (const bfd_arch_info_type *a, const bfd_arch_info_type *b)
{
  BFD_ASSERT (a->arch == bfd_arch_sx);
  switch (b->arch)
    {
    default:
      return NULL;
    case bfd_arch_sx:
      return bfd_default_compatible (a, b);
    }
  /*NOTREACHED*/
}

const bfd_arch_info_type bfd_sx_arch[] =
{
  {
    32, /* 32 bits in a word */
    64, /* 64 bits in an address */
    8,  /* 8 bits in a byte */
    bfd_arch_sx,        /* architecture */
    bfd_mach_sx3,       /* machine */
    "sx",               /* architecture name */
    "sx3",              /* printable name */
    3,                  /* aligned power */
    FALSE,              /* the default machine for the architecture */
    sx_compatible,      /* the SX is only compatible with itself, see above */
    bfd_default_scan,
    &bfd_sx_arch[1],
  },

  {
    32,	/* 32 bits in a word */
    64,	/* 64 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_sx,	/* architecture */
    bfd_mach_sx4,	/* machine */
    "sx",		/* architecture name */
    "sx4",		/* printable name */
    3,			/* aligned power */
    FALSE, 		/* the default machine for the architecture */
    sx_compatible,	/* the SX is only compatible with itself, see above */
    bfd_default_scan,
    &bfd_sx_arch[2],
  },

  {
    32,	/* 32 bits in a word */
    64,	/* 64 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_sx,	/* architecture */
    bfd_mach_sx5,	/* machine */
    "sx",		/* architecture name */
    "sx5",		/* printable name */
    3,			/* aligned power */
    FALSE, 		/* the default machine for the architecture */
    sx_compatible,	/* the SX is only compatible with itself, see above */
    bfd_default_scan,
    &bfd_sx_arch[3],
  },

  {
    32,	/* 32 bits in a word */
    64,	/* 64 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_sx,	/* architecture */
    bfd_mach_sx8,	/* machine */
    "sx",		/* architecture name */
    "sx8",		/* printable name */
    3,			/* aligned power */
    TRUE, 		/* the default machine for the architecture */
    sx_compatible,	/* the SX is only compatible with itself, see above */
    bfd_default_scan,
    NULL,
  },

  {
    32,	/* 32 bits in a word */
    64,	/* 64 bits in an address */
    8,	/* 8 bits in a byte */
    bfd_arch_sx,	/* architecture */
    bfd_mach_sx9,	/* machine */
    "sx",		/* architecture name */
    "sx9",		/* printable name */
    3,			/* aligned power */
    TRUE, 		/* the default machine for the architecture */
    sx_compatible,	/* the SX is only compatible with itself, see above */
    bfd_default_scan,
    NULL,
  },

};

/* COFF flavour for SUPER-UX on NEC SX machines.

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


#include "sysdep.h"
#include "bfd.h"
#include "bfdlink.h"
#include "libbfd.h"

/* these need to be defined prior to #including internal.h */
#define FILNMLEN 36
#define SYMNMLEN 16

#include "coff/internal.h"
#include "coff/sx.h"
#include "aout/ar.h"

#include "libcoff.h"

#define BADMAG(x) ( (x).f_magic != SX3MAGIC && \
		    (x).f_magic != SX4MAGIC && \
		    (x).f_magic != SX5MAGIC && \
		    (x).f_magic != SX8MAGIC) 
#define __A_MAGIC_SET__
/* this symbol serves us to conditionally compile in parts of
   coffcode.h and coffswap.h */
#define NEC_SX_COFF 1

#ifndef TARGET_BIG_SYM
#define TARGET_BIG_SYM sxcoff_big_vec
#endif

#ifndef TARGET_BIG_NAME
/* note that the target should have "64" in its name so that
   various utils (such as nm) can determine the address size
   (only ELF stores address size in the header, various COFF
   flavours don't)
*/
#define TARGET_BIG_NAME COFF_SX_TARGET_BIG_NAME
#endif

#define COFF_DEFAULT_SECTION_ALIGNMENT_POWER (3)

/* file header */
#define GET_FILEHDR_SYMPTR H_GET_64
#define PUT_FILEHDR_SYMPTR H_PUT_64
#define GET_FILEHDR_NSCNS  H_GET_32
#define PUT_FILEHDR_NSCNS  H_PUT_32
#define GET_FILEHDR_NSYMS  H_GET_64
#define PUT_FILEHDR_NSYMS  H_PUT_64
#define GET_FILEHDR_OPTHDR H_GET_32
#define PUT_FILEHDR_OPTHDR H_PUT_32

/* a.out header */
#define GET_AOUTHDR_TSIZE H_GET_64
#define PUT_AOUTHDR_TSIZE H_PUT_64
#define GET_AOUTHDR_DSIZE H_GET_64
#define PUT_AOUTHDR_DSIZE H_PUT_64
#define GET_AOUTHDR_BSIZE H_GET_64
#define PUT_AOUTHDR_BSIZE H_PUT_64
#define GET_AOUTHDR_ENTRY H_GET_64
#define PUT_AOUTHDR_ENTRY H_PUT_64
#define GET_AOUTHDR_TEXT_START H_GET_64
#define PUT_AOUTHDR_TEXT_START H_PUT_64
#define GET_AOUTHDR_DATA_START H_GET_64
#define PUT_AOUTHDR_DATA_START H_PUT_64

/* section header */
#define GET_SCNHDR_PADDR H_GET_64
#define PUT_SCNHDR_PADDR H_PUT_64
#define GET_SCNHDR_VADDR H_GET_64
#define PUT_SCNHDR_VADDR H_PUT_64
#define GET_SCNHDR_SIZE H_GET_64
#define PUT_SCNHDR_SIZE H_PUT_64
#define GET_SCNHDR_SCNPTR H_GET_64
#define PUT_SCNHDR_SCNPTR H_PUT_64
#define GET_SCNHDR_RELPTR H_GET_64
#define PUT_SCNHDR_RELPTR H_PUT_64
#define GET_SCNHDR_LNNOPTR H_GET_64
#define PUT_SCNHDR_LNNOPTR H_PUT_64
#define GET_SCNHDR_NRELOC H_GET_32
#define MAX_SCNHDR_NRELOC 0xffffffff
#define PUT_SCNHDR_NRELOC H_PUT_32
#define GET_SCNHDR_NLNNO H_GET_32
#define MAX_SCNHDR_NLNNO 0xffffffff
#define PUT_SCNHDR_NLNNO H_PUT_32
#define GET_SCNHDR_FLAGS H_GET_32
#define PUT_SCNHDR_FLAGS H_PUT_32

/* relocation entry */
#define GET_RELOC_VADDR H_GET_64
#define PUT_RELOC_VADDR H_PUT_64

/* symbol entry */
#define GET_SYM_OFFSET  H_GET_64
#define GET_SYM_VALUE   H_GET_64
#define GET_SYM_SCNUM   H_GET_32
#define PUT_SYM_OFFSET  H_PUT_64
#define PUT_SYM_VALUE   H_PUT_64
#define PUT_SYM_SCNUM   H_PUT_32

/* hook for adjusting output syment */
#define COFF_ADJUST_SYM_OUT_POST(abfd, inp, expt) \
  coff_sx_adjust_sym_out_post(abfd, inp, expt)

/* lineno entry */
#define GET_LINENO_LNNO(abfd, ext) \
  H_GET_32 (abfd,      (ext->l_lnno))
#define PUT_LINENO_LNNO(abfd, val, ext) \
  H_PUT_32 (abfd, val, (ext->l_lnno))
#define GET_LINENO_SYMNDX(abfd, ext) \
  H_GET_64 (abfd,      (ext->l_addr.l_symndx))
#define PUT_LINENO_SYMNDX(abfd, val, ext) \
  H_PUT_64 (abfd, val, (ext->l_addr.l_symndx))

/* aux entry ... whew, whole lot of these ... */
#define GET_SCN_SCNLEN(abfd, ext) \
  H_GET_64 (abfd, ext->x_scn.x_scnlen)
#define GET_SCN_NRELOC(abfd, ext) \
  H_GET_32 (abfd, ext->x_scn.x_nreloc)
#define GET_SCN_NLINNO(abfd, ext) \
  H_GET_32 (abfd, ext->x_scn.x_nlinno)
#define PUT_SCN_SCNLEN(abfd, in, ext) \
  H_PUT_64 (abfd, in, ext->x_scn.x_scnlen)
#define PUT_SCN_NRELOC(abfd, in, ext) \
  H_PUT_32 (abfd, in, ext->x_scn.x_nreloc)
#define PUT_SCN_NLINNO(abfd, in, ext) \
  H_PUT_32 (abfd, in, ext->x_scn.x_nlinno)
#define GET_FCN_LNNOPTR(abfd, ext) \
  H_GET_64 (abfd, ext->x_sym.x_fcnary.x_fcn.x_lnnoptr)
#define GET_FCN_ENDNDX(abfd, ext) \
  H_GET_32 (abfd, ext->x_sym.x_fcnary.x_fcn.x_endndx)
#define PUT_FCN_LNNOPTR(abfd, in, ext) \
  H_PUT_64 (abfd,  in, ext->x_sym.x_fcnary.x_fcn.x_lnnoptr)
#define PUT_FCN_ENDNDX(abfd, in, ext) \
  H_PUT_32 (abfd, in, ext->x_sym.x_fcnary.x_fcn.x_endndx)
#define GET_FSIZE(abfd, ext) \
  H_GET_64 (abfd, ext->x_sym.x_misc.x_fsize)
#define PUT_FSIZE(abfd, in, ext) \
  H_PUT_64(abfd, in, ext->x_sym.x_misc.x_fsize)
#define GET_LNSZ_LNNO(abfd, ext) \
  H_GET_32(abfd, ext->x_sym.x_misc.x_lnsz.x_lnno)
#define GET_LNSZ_SIZE(abfd, ext) \
  H_GET_32(abfd, ext->x_sym.x_misc.x_lnsz.x_size)
#define PUT_LNSZ_LNNO(abfd, in, ext) \
  H_PUT_32(abfd, in, ext->x_sym.x_misc.x_lnsz.x_lnno)
#define PUT_LNSZ_SIZE(abfd, in, ext) \
  H_PUT_32(abfd, in, ext->x_sym.x_misc.x_lnsz.x_size)
#define GET_ARY_DIM(abfd, ext, n) \
  H_GET_32(abfd, ext->x_sym.x_fcnary.x_ary.x_dimen[n]);
#define PUT_ARY_DIM(abfd, in, ext, n) \
  H_PUT_32(abfd, in, ext->x_sym.x_fcnary.x_ary.x_dimen[n]);

/* will probably need to add more due to bizarre SX aux entries ... */

#ifdef SX_LANG_FROM_FNAME
static const char *
coff_sx_internal_auxent_fname (bfd *abfd,
                               const union internal_auxent *aux,
                               char *buf)
{
  if (aux->x_file.x_n.x_zeroes != 0
      || aux->x_file.x_n.x_offset == 0)
    {
      memcpy (buf, aux->x_file.x_fname, bfd_coff_filnmlen(abfd));
      buf[bfd_coff_filnmlen(abfd)] = '\0';
      return buf;
    }
  else
    {
      const char *strings;

      BFD_ASSERT (aux->x_file.x_n.x_offset >= STRING_SIZE_SIZE);
      strings = obj_coff_strings (abfd);
      if (strings == NULL)
        {
          strings = _bfd_coff_read_string_table (abfd);
          if (strings == NULL)
            return NULL;
        }
      return strings + aux->x_file.x_n.x_offset;
    }
}
#endif /* SX_LANG_FROM_FNAME */

/* syment handling */
static void
coff_sx_adjust_sym_out_post (bfd *abfd ATTRIBUTE_UNUSED,
                             void *inp ATTRIBUTE_UNUSED,
                             void *extp)
{
  SYMENT *ext = (SYMENT *) extp;

#ifdef SX_LANG_FROM_FNAME
  struct internal_syment *in = (struct internal_syment *) inp;

#define COMBINED_ENTRY_FROM_SYMENT(s)					\
  (combined_entry_type *)						\
    (((char *)s) - (char *)&(((combined_entry_type *)0)->u.syment))

  if((in->n_sclass == C_FILE) &&
     (in->n_numaux > 0))
    {
      /* first some heavy pointer magic ... we know that inp comes
         from a member of an array of combined_entry_type */
      combined_entry_type *native = COMBINED_ENTRY_FROM_SYMENT(in);
      union internal_auxent *aux = &(native[1].u.auxent);
      char buf[MAX_FILNMLEN + 1];
      const char *name = coff_sx_internal_auxent_fname(abfd, aux, buf);

      char *sfx_start = strrchr(name, '.');
      if(NULL != sfx_start)
        {          
          if((0 == strncmp(sfx_start, ".c", 2)) ||
             (0 == strncmp(sfx_start, ".h", 2)))
            {
              ext->_e_misc[0] = L_C;
              ext->_e_misc[1] = 0;
            }
          else if((0 == strncmp(sfx_start, ".C", 2)) ||
                   (0 == strncmp(sfx_start, ".cpp", 4)))
            {
              ext->_e_misc[0] = L_C;
              ext->_e_misc[1] = (1 << 7);
            }
          else if(0 == strncmp(sfx_start, ".f", 2))
            {
              ext->_e_misc[0] = L_F77;
              ext->_e_misc[1] = 0;
            }
          else
            {
              ext->_e_misc[0] = L_UNKNOWN;
              ext->_e_misc[1] = 0;
            }
        }
    }
#else
  ext->_e_misc[0] = L_C;
  ext->_e_misc[1] = 0;
#endif /* SX_LANG_FROM_FNAME */
}


/* archive file data structures */

typedef struct sx_coff_ar_file_hdr {
#define SX_COFF_AR_HDR_NAME_S 64
  char ar_name[SX_COFF_AR_HDR_NAME_S]; /* name of this member */
#define SX_COFF_AR_HDR_DATE_S 12
  char ar_date[SX_COFF_AR_HDR_DATE_S]; /* file mtime */
#define SX_COFF_AR_HDR_UID_S 6
  char ar_uid[SX_COFF_AR_HDR_UID_S];   /* owner uid; printed as decimal */
#define SX_COFF_AR_HDR_GID_S 6
  char ar_gid[SX_COFF_AR_HDR_GID_S];   /* owner gid; printed as decimal */
#define SX_COFF_AR_HDR_MODE_S 8
  char ar_mode[SX_COFF_AR_HDR_MODE_S]; /* file mode, printed as octal   */
#define SX_COFF_AR_HDR_SIZE_S 22       
  char ar_size[SX_COFF_AR_HDR_SIZE_S]; /* file size, printed as decimal */
#define SX_COFF_AR_HDR_FMAG_S 2
  char ar_fmag[SX_COFF_AR_HDR_FMAG_S]; /* should contain SX_COFF_AR_HDR_FMAG */
} sx_coff_ar_file_hdr;

/* now, this is a mighty fine way to compute an offset! ;) */
#define SX_COFF_AR_HDR_FMAG_OFF \
  ((long)&(((sx_coff_ar_file_hdr *)NULL)->ar_fmag))

#define SX_COFF_AR_MAG    "!<arch>\012"
#define SX_COFF_AR_MAG_S  8

#define SX_COFF_AR_HDR_FMAG   "\140\012"

#define SX_COFF_AR_HDR_ARMAP_FILENAME "/                     "

#define coff_sx_arch_eltdata(bfd) \
  ((struct areltdata *) ((bfd)->arelt_data))
#define coff_sx_arch_hdr(bfd) \
  ((sx_coff_ar_file_hdr *) coff_sx_arch_eltdata(bfd)->arch_header)
#define coff_sx_ar_padchar(abfd) ((abfd)->xvec->ar_pad_char)

/* archive handling routines */

/* check if the archive is empty (i.e. there are no object and other 
  files in it). We do this by checking if an EOF occurs just after the 
  global header (i.e. !<arch>.) 
  
  This function return 0 on success and sets the value of "is_empty" variable
  to TRUE if the archive is empty */
static bfd_boolean
is_archive_empty(bfd *abfd, bfd_boolean *is_empty)
{
  /* FIXIT: this way of determining if EOF was reached is really ugly.
     Does BFD perhaps enable us to do EOF checking in a smarter way?? */
     
  char test_buff[1];
  if (bfd_bread(test_buff, 1, abfd) == 0) 
    {
      /* archive file is empty */
      *is_empty = TRUE;
    }
  else 
    {
      /* if file is not empty, jump back to the previous position */
      if(0 != bfd_seek(abfd, (file_ptr) -1, SEEK_CUR))
        {
          SX_DEBUG("Seek failed.\n");
          if (bfd_get_error () != bfd_error_system_call)
            bfd_set_error (bfd_error_wrong_format);
          return -1;
        }
      *is_empty = FALSE;
    }
  return 0;
}

/* recognizing the SX COFF: header is longer than normal GNU/BSD/SYSV style
   archives */
static const bfd_target *
coff_sx_archive_p (bfd *abfd)
{
  struct artdata *tdata_hold;
  char armag[SX_COFF_AR_MAG_S + 1], farmag[SX_COFF_AR_HDR_FMAG_S + 1];
  bfd_size_type amt;
  bfd_boolean arc_empty = FALSE;

  if (bfd_bread (armag, SX_COFF_AR_MAG_S, abfd) != SX_COFF_AR_MAG_S)
    {
      if (bfd_get_error () != bfd_error_system_call)
        bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  if (strncmp (armag, SX_COFF_AR_MAG, SX_COFF_AR_MAG_S) != 0)
    {
      SX_DEBUG("Archive magic does not match.\n");
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }
  
  if (is_archive_empty(abfd, &arc_empty) != 0) 
    {
      SX_DEBUG("Testing archive for emptiness failed.\n");
      if (bfd_get_error () != bfd_error_system_call)
        bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }   

  if (!arc_empty) {
    /* now we need to check if the header size matches the non-standard
       SX/COFF header size. we do this by checking if the file header magic
       occurs where we expect it to. */
    /* TODO: is it perhaps possible that the *second* file header magic in
       an ordinary COFF archive occurs at this point? if so, we need to check
       that no file header magic occurs in the previous bytes ... need to think
       about it and fix it if necessary ... */
    if(0 != bfd_seek(abfd, (file_ptr) SX_COFF_AR_HDR_FMAG_OFF, SEEK_CUR))
      {
        SX_DEBUG("Seek failed.\n");
        if (bfd_get_error () != bfd_error_system_call)
          bfd_set_error (bfd_error_wrong_format);
        return NULL;
      }
    if(SX_COFF_AR_HDR_FMAG_S != bfd_bread (farmag, SX_COFF_AR_HDR_FMAG_S, abfd))
      {
        SX_DEBUG("Reading farmag failed.\n");
        if (bfd_get_error () != bfd_error_system_call)
          bfd_set_error (bfd_error_wrong_format);
        return NULL;
      }
      
  
    if(0 != memcmp(farmag, SX_COFF_AR_HDR_FMAG, SX_COFF_AR_HDR_FMAG_S))
      {
        SX_DEBUG("Farmag does not match. Got %02x%02x\n",
                 (unsigned char)farmag[0], (unsigned char)farmag[1]);
        bfd_set_error (bfd_error_wrong_format);
        return NULL;
      }
      
    /* jump back to the start of the header */
    if(0 != bfd_seek(abfd, (file_ptr) -(SX_COFF_AR_HDR_FMAG_OFF + 
                                        SX_COFF_AR_HDR_FMAG_S), SEEK_CUR)) 
      {
        SX_DEBUG("Failed to seek back.\n");
        return NULL;
      }
  }

  tdata_hold = bfd_ardata (abfd);

  amt = sizeof (struct artdata);
  bfd_ardata (abfd) = bfd_zalloc (abfd, amt);
  if (bfd_ardata (abfd) == NULL)
    {
      bfd_ardata (abfd) = tdata_hold;
      return NULL;
    }

  /* this is where the first file in archive begins ... */
  bfd_ardata (abfd)->first_file_filepos = SX_COFF_AR_MAG_S;

  /* NOTE: no need to parse extended name table, as SX COFF flavour of
     archives does not support it ... */
  if (!BFD_SEND (abfd, _bfd_slurp_armap, (abfd)))
    {
      if (bfd_get_error () != bfd_error_system_call)
        bfd_set_error (bfd_error_wrong_format);
      bfd_release (abfd, bfd_ardata (abfd));
      bfd_ardata (abfd) = tdata_hold;
      return NULL;
    }

  if (bfd_has_map (abfd))
    {
      bfd *first;

      /* This archive has a map, so we may presume that the contents
         are object files.  Make sure that if the first file in the
         archive can be recognized as an object file, it is for this
         target.  If not, assume that this is the wrong format.  If
         the first file is not an object file, somebody is doing
         something weird, and we permit it so that ar -t will work.
         
         This is done because any normal format will recognize any
         normal archive, regardless of the format of the object files.
         We do accept an empty archive.  */

      first = bfd_openr_next_archived_file (abfd, NULL);
      if (first != NULL)
        {
          first->target_defaulted = FALSE;
          if (bfd_check_format (first, bfd_object)
              && first->xvec != abfd->xvec)
            {
              bfd_set_error (bfd_error_wrong_object_format);
              /* generic impl does not release ardata, however, I think
                 we should ... */
              bfd_release (abfd, bfd_ardata (abfd));
              bfd_ardata (abfd) = tdata_hold;
              return NULL;
            }
        }
    }

  return abfd->xvec;
}

/* slurping and constructing the extended name table is not supported as
   SX COFF seems not to support an extended name table */
static bfd_boolean
coff_sx_slurp_extended_name_table(bfd *abfd ATTRIBUTE_UNUSED)
{
  SX_DEBUG("SX COFF does not have an extended name table. "
           "slurp_extended_name_table routine should "
           "*never* be called.\n");
  abort();
  return FALSE;
}

static bfd_boolean
coff_sx_construct_extended_name_table (bfd *abfd ATTRIBUTE_UNUSED,
                                       char **tabloc ATTRIBUTE_UNUSED,
                                       bfd_size_type *table ATTRIBUTE_UNUSED,
                                       const char **name ATTRIBUTE_UNUSED)
{
  SX_DEBUG("SX COFF does not have an extended name table. "
           "construct_extended_name_table routine should "
           "*never* be called.\n");
  abort();
  return FALSE;
}

#define ar_maxnamelen(abfd) ((abfd)->xvec->ar_max_namelen) 

static void *
coff_sx_read_ar_hdr(bfd *abfd)
{
  /* due to different header size of SX coff archives, we have to define our 
     own struct for storing the header data. */
  sx_coff_ar_file_hdr hdr; 
  char *hdrp = (char *) &hdr;
  size_t parsed_size;
  struct areltdata *ared;
  bfd_size_type namelen = 0;
  bfd_size_type allocsize = sizeof (struct areltdata) +
    sizeof (sx_coff_ar_file_hdr);
  char *allocptr = 0;

  if (bfd_bread (hdrp, sizeof (sx_coff_ar_file_hdr), abfd) !=
      sizeof (sx_coff_ar_file_hdr))
    {
      if (bfd_get_error () != bfd_error_system_call)
        bfd_set_error (bfd_error_no_more_archived_files);
      return NULL;
    }

  if (strncmp (hdr.ar_fmag, SX_COFF_AR_HDR_FMAG, 2) != 0)
    {
      SX_DEBUG("Archive file header doesn't comply to SX coff fromat.\n");
      bfd_set_error (bfd_error_malformed_archive);
      return NULL;
    }

  errno = 0;
  parsed_size = strtol (hdr.ar_size, NULL, 10);
  if (errno != 0)
    {
      bfd_set_error (bfd_error_malformed_archive);
      return NULL;
    }

  /* Extract the filename from the archive: max 64 chars on SX COFF
     We judge the end of the name by looking for '/'. (SYSV style) */

  char *e;
  e = memchr (hdr.ar_name, '/', ar_maxnamelen (abfd));
  if (e != NULL)
    namelen = e - hdr.ar_name;
  else
    {
      /* If we didn't find a termination character, then the name
         must be the entire field.  */
      namelen = ar_maxnamelen (abfd);
    }

  allocsize += namelen + 1;

  if (!allocptr)
    {
      allocptr = bfd_zalloc (abfd, allocsize);
      if (allocptr == NULL)
        return NULL;
    }

  ared = (struct areltdata *) allocptr;

  ared->arch_header = allocptr + sizeof (struct areltdata);
  memcpy (ared->arch_header, &hdr, sizeof (sx_coff_ar_file_hdr));
  ared->parsed_size = parsed_size;

  /* extract the object filename (for the archive header, the name can be 
     empty) */
  ared->filename = allocptr + (sizeof (struct areltdata) +
                               sizeof (sx_coff_ar_file_hdr));
  if (namelen)
    memcpy (ared->filename, hdr.ar_name, namelen);
  ared->filename[namelen] = '\0';

  return ared;
}


/* Analogous to stat call.  */

static int
coff_sx_generic_stat_arch_elt (bfd *abfd, struct stat *buf)
{
  sx_coff_ar_file_hdr *hdr;
  char *aloser;

  if (abfd->arelt_data == NULL)
    {
      bfd_set_error (bfd_error_invalid_operation);
      return -1;
    }

  hdr = coff_sx_arch_hdr (abfd);

#define foo(arelt, stelt, size)                     \
  buf->stelt = strtol (hdr->arelt, &aloser, size);  \
  if (aloser == hdr->arelt)                         \
    return -1;

  foo (ar_date, st_mtime, 10);
  foo (ar_uid, st_uid, 10);
  foo (ar_gid, st_gid, 10);  
  foo (ar_mode, st_mode, 8);

  buf->st_size = coff_sx_arch_eltdata (abfd)->parsed_size;

  return 0;
}

/* A SX coff armap looks like :
   lARMAG
   sx_coff_ar_file_hdr with name = '/'
   number of symbols
   offset of file for symbol 0
   offset of file for symbol 1

   offset of file for symbol n-1
   symbol name 0
   symbol name 1

   symbol name n-1  */

static bfd_boolean
coff_sx_write_armap (bfd *arch,
      unsigned int elength,
      struct orl *map,
      unsigned int symbol_count,
      int stridx)
{
  /* The size of the ranlib is the number of exported symbols in the
     archive * the number of bytes in an int, + an int for the count.  */
  unsigned int ranlibsize = (symbol_count * 8) + 4;
  unsigned int stringsize = stridx;
  unsigned int mapsize = stringsize + ranlibsize;
  unsigned long long archive_member_file_ptr;
  bfd *current = arch->archive_head;
  unsigned int count;
  sx_coff_ar_file_hdr hdr;
  int padit = mapsize & 1;
  
  if (padit)
    mapsize++;

  /* Work out where the first object file will go in the archive.  */
  archive_member_file_ptr = (mapsize
                             + elength
                             + sizeof (sx_coff_ar_file_hdr)
                             + SARMAG);

  memset (&hdr, ' ', sizeof (sx_coff_ar_file_hdr));
  hdr.ar_name[0] = '/';
  _bfd_ar_spacepad (hdr.ar_size, sizeof (hdr.ar_size), "%-22ld",
                    mapsize);
  _bfd_ar_spacepad (hdr.ar_date, sizeof (hdr.ar_date), "%ld",
                    time (NULL));
  /* This, at least, is what Intel coff sets the values to.  */
  _bfd_ar_spacepad (hdr.ar_uid, sizeof (hdr.ar_uid), "%ld", 0);
  _bfd_ar_spacepad (hdr.ar_gid, sizeof (hdr.ar_gid), "%ld", 0);
  _bfd_ar_spacepad (hdr.ar_mode, sizeof (hdr.ar_mode), "%-7lo", 0);
  memcpy (hdr.ar_fmag, ARFMAG, 2);

  /* Write the ar header for this item and the number of symbols.  */
  if (bfd_bwrite (&hdr, sizeof (sx_coff_ar_file_hdr), arch)
      != sizeof (sx_coff_ar_file_hdr))
    return FALSE;

  if (!bfd_write_bigendian_4byte_int (arch, symbol_count))
    return FALSE;

  /* Two passes, first write the file offsets for each symbol -
     remembering that each offset is on a two byte boundary.  */

  /* Write out the file offset for the file associated with each
     symbol, and remember to keep the offsets padded out.  */

  current = arch->archive_head;
  count = 0;
  while (current != NULL && count < symbol_count)
    {
      /* For each symbol which is used defined in this object, write
         out the object file's address in the archive.  */

      while (count < symbol_count && map[count].u.abfd == current)
        {
          if (!bfd_write_bigendian_8byte_long_long(arch,
                                                   archive_member_file_ptr))
            return FALSE;
          count++;
        }
      
      /* Add size of this archive entry.  */
      archive_member_file_ptr += arelt_size (current) +
        sizeof (sx_coff_ar_file_hdr);
      /* Remember aboout the even alignment.  */
      archive_member_file_ptr += archive_member_file_ptr % 2;
      current = current->archive_next;
    }

  /* Now write the strings themselves.  */
  for (count = 0; count < symbol_count; count++)
    {
      size_t len = strlen (*map[count].name) + 1;

      if (bfd_bwrite (*map[count].name, len, arch) != len)
        return FALSE;
    }

  /* The spec sez this should be a newline.  But in order to be
     bug-compatible for arc960 we use a null.  */
  if (padit)
    {
      if (bfd_bwrite ("", 1, arch) != 1)
        return FALSE;
    }

  return TRUE;
}

static const char *
sx_coff_normalize (bfd *abfd ATTRIBUTE_UNUSED, const char *file)
{
  const char *filename = strrchr (file, '/');

  if (filename != NULL)
    filename++;
  else
    filename = file;
  return filename;
}

static void
coff_sx_truncate_arname (bfd *abfd, const char *pathname, char *arhdr)
{
  sx_coff_ar_file_hdr *hdr = (sx_coff_ar_file_hdr *) arhdr;
  size_t length;
  const char *filename;
  size_t maxlen = ar_maxnamelen (abfd);

  if ((bfd_get_file_flags (abfd) & BFD_TRADITIONAL_FORMAT) != 0)
    {
      SX_DEBUG("SX archives should not have traditional format flag set.");
      abort();
    }

  filename = sx_coff_normalize (abfd, pathname);
  if (filename == NULL)
    {
      /* FIXME */
      abort ();
    }

  length = strlen (filename);

  if (length <= maxlen)
    memcpy (hdr->ar_name, filename, length);

  /* Add the padding character if there is room for it.  */
  if (length < maxlen
      || (length == maxlen && length < sizeof hdr->ar_name))
    (hdr->ar_name)[length] = coff_sx_ar_padchar (abfd);
}

/* Takes a filename, returns an arelt_data for it, or NULL if it can't
   make one.  The filename must refer to a filename in the filesystem.
   The filename field of the ar_hdr will NOT be initialized.  If member
   is set, and it's an in-memory bfd, we fake it.  */
static struct areltdata *
coff_sx_ar_hdr_from_filesystem (bfd *abfd, const char *filename, 
                                bfd *member)
{
  struct stat status;
  struct areltdata *ared;
  sx_coff_ar_file_hdr *hdr;
  bfd_size_type amt;

  if (member && (member->flags & BFD_IN_MEMORY) != 0)
    {
      /* Assume we just "made" the member, and fake it.  */
      struct bfd_in_memory *bim = member->iostream;
      time (&status.st_mtime);
      status.st_uid = getuid ();
      status.st_gid = getgid ();
      status.st_mode = 0644;
      status.st_size = bim->size;
    }
  else if (stat (filename, &status) != 0)
    {
      bfd_set_error (bfd_error_system_call);
      return NULL;
    }

  amt = sizeof (sx_coff_ar_file_hdr) + sizeof (struct areltdata);
  ared = bfd_zalloc (abfd, amt);
  if (ared == NULL)
    return NULL;
  hdr = (sx_coff_ar_file_hdr *) (((char *) ared) + sizeof (struct areltdata));

  /* ar headers are space padded, not null padded!  */
  memset (hdr, ' ', sizeof (sx_coff_ar_file_hdr));

  _bfd_ar_spacepad (hdr->ar_date, sizeof (hdr->ar_date), "%-12ld",
                    status.st_mtime);
  _bfd_ar_spacepad (hdr->ar_uid, sizeof (hdr->ar_uid), "%ld",
                    status.st_uid);
  _bfd_ar_spacepad (hdr->ar_gid, sizeof (hdr->ar_gid), "%ld",
                    status.st_gid);
  _bfd_ar_spacepad (hdr->ar_mode, sizeof (hdr->ar_mode), "%-8lo",
                    status.st_mode);
  _bfd_ar_spacepad (hdr->ar_size, sizeof (hdr->ar_size), "%-22ld",
                    status.st_size);
  memcpy (hdr->ar_fmag, ARFMAG, 2);
  ared->parsed_size = status.st_size;
  ared->arch_header = (char *) hdr;

  return ared;
}

static bfd_boolean
coff_sx_slurp_armap (bfd *abfd)
{
#define NAMELEN SX_COFF_AR_HDR_SIZE_S + 1

  /* here, the general coff archives will use only the first 17 bytes for 
     storing filename. */
  char nextname[NAMELEN];
  int r, reallen;

  reallen = NAMELEN;
  
  r = bfd_bread (nextname, reallen - 1, abfd);

  if (r == 0)
    return TRUE;
  if (r != reallen - 1)
    return FALSE;

  if (bfd_seek (abfd, (file_ptr) -(reallen - 1), SEEK_CUR) != 0)
    return FALSE;

  if (CONST_STRNEQ(nextname, SX_COFF_AR_HDR_ARMAP_FILENAME))
    {
      struct areltdata *mapdata;
      char *raw_armap, *rawptr;
      struct artdata *ardata = bfd_ardata (abfd);
      char *stringbase;
      bfd_size_type stringsize;
      unsigned int parsed_size;
      carsym *carsyms;
      bfd_size_type nsymz;		/* Number of symbols in armap.  */
      bfd_vma (*swap) (const void *);
      char int_buf[sizeof (long)];
      bfd_size_type carsym_size, ptrsize;
      unsigned int i;
      unsigned int off_size;

      /* SX COFF has 64-bit offsets into archive ... */
      off_size = 8;
      swap = bfd_getb64;

      mapdata = _bfd_read_ar_hdr (abfd);
      if (mapdata == NULL)
        return FALSE;
      parsed_size = mapdata->parsed_size;
      bfd_release (abfd, mapdata);	/* Don't need it any more.  */
      
      if (bfd_bread (int_buf, 4, abfd) != 4)
        {
          if (bfd_get_error () != bfd_error_system_call)
            bfd_set_error (bfd_error_malformed_archive);
          return FALSE;
        }
      /* It seems that all numeric information in a coff archive is always
         in big endian format, nomatter the host or target.  */
      nsymz = bfd_getb32 (int_buf);
      stringsize = parsed_size - (off_size * nsymz) - 4;
      
      /* ... except that some archive formats are broken, and it may be our
         fault - the i960 little endian coff sometimes has big and sometimes
         little, because our tools changed.  Here's a horrible hack to clean
         up the crap.  */
      
      if (stringsize > 0xfffff
          && bfd_get_arch (abfd) == bfd_arch_i960
          && bfd_get_flavour (abfd) == bfd_target_coff_flavour)
        {
          /* This looks dangerous, let's do it the other way around.  */
          nsymz = bfd_getl32 (int_buf);
          stringsize = parsed_size - (off_size * nsymz) - 4;
          swap = bfd_getl32;
        }
      
      /* The coff armap must be read sequentially.  So we construct a
         bsd-style one in core all at once, for simplicity.  */
      
      /* TODO: is this check affected by 64-bit offsets in SX COFF? */
      if (nsymz > ~ (bfd_size_type) 0 / sizeof (carsym))
        return FALSE;

      carsym_size = (nsymz * sizeof (carsym));
      ptrsize = (off_size * nsymz);

      if (carsym_size + stringsize + 1 <= carsym_size)
        return FALSE;

      ardata->symdefs = bfd_zalloc (abfd, carsym_size + stringsize + 1);
      if (ardata->symdefs == NULL)
        return FALSE;
      carsyms = ardata->symdefs;
      stringbase = ((char *) ardata->symdefs) + carsym_size;
      
      /* Allocate and read in the raw offsets.  */
      raw_armap = bfd_alloc (abfd, ptrsize);
      if (raw_armap == NULL)
        goto release_symdefs;
      if (bfd_bread (raw_armap, ptrsize, abfd) != ptrsize
          || (bfd_bread (stringbase, stringsize, abfd) != stringsize))
        {
          if (bfd_get_error () != bfd_error_system_call)
            bfd_set_error (bfd_error_malformed_archive);
          goto release_raw_armap;
        }
      
      /* OK, build the carsyms.  */
      for (i = 0; i < nsymz; i++)
        {
          rawptr = raw_armap + off_size*i;
          carsyms->file_offset = swap ((bfd_byte *) rawptr);
          carsyms->name = stringbase;
          stringbase += strlen (stringbase) + 1;
          carsyms++;
        }
      *stringbase = 0;
      
      ardata->symdef_count = nsymz;
      ardata->first_file_filepos = bfd_tell (abfd);
      /* Pad to an even boundary if you have to.  */
      ardata->first_file_filepos += (ardata->first_file_filepos) % 2;
      
      bfd_has_map (abfd) = TRUE;
      bfd_release (abfd, raw_armap);
      
      /* Check for a second archive header (as used by PE).  */
      {
        struct areltdata *tmp;

        bfd_seek (abfd, ardata->first_file_filepos, SEEK_SET);
        tmp = _bfd_read_ar_hdr (abfd);
        if (tmp != NULL)
          {
            if (tmp->arch_header[0] == '/'
                && tmp->arch_header[1] == ' ')
              {
                ardata->first_file_filepos +=
                  (tmp->parsed_size + sizeof (struct ar_hdr) + 1) &
                  ~(unsigned) 1;
              }
            bfd_release (abfd, tmp);
          }
      }
      
      return TRUE;
      
    release_raw_armap:
      bfd_release (abfd, raw_armap);
    release_symdefs:
      bfd_release (abfd, (ardata)->symdefs);
      return FALSE;
    }

  bfd_has_map (abfd) = FALSE;
  return TRUE;
}

static bfd_boolean
coff_sx_write_archive_contents (bfd *arch)
{
  bfd *current;
  bfd_boolean makemap = bfd_has_map (arch);
  /* If no .o's, don't bother to make a map.  */
  bfd_boolean hasobjects = FALSE;
  bfd_size_type wrote;
  int tries;

  /* Verify the viability of all entries; if any of them live in the
     filesystem (as opposed to living in an archive open for input)
     then construct a fresh ar_hdr for them.  */
  for (current = arch->archive_head;
       current != NULL;
       current = current->archive_next)
    {
      /* This check is checking the bfds for the objects we're reading
         from (which are usually either an object file or archive on
         disk), not the archive entries we're writing to.  We don't
         actually create bfds for the archive members, we just copy
         them byte-wise when we write out the archive.  */
      if (bfd_write_p (current))
        {
          bfd_set_error (bfd_error_invalid_operation);
          goto input_err;
        }
      if (!current->arelt_data)
        {
          current->arelt_data =
            coff_sx_ar_hdr_from_filesystem (arch, current->filename, current);
          if (!current->arelt_data)
            goto input_err;
          
          /* Put in the file name.  */
          BFD_SEND (arch, _bfd_truncate_arname,
                    (arch, current->filename,
                     (char *) coff_sx_arch_hdr(current)));
        }
      
      if (makemap && ! hasobjects)
        {			/* Don't bother if we won't make a map!  */
          if ((bfd_check_format (current, bfd_object)))
            hasobjects = TRUE;
        }
    }
  
  
  /* since coff SX doesn't support extended names table, we shouldn't try to 
     write it. */
  
  if (bfd_seek (arch, (file_ptr) 0, SEEK_SET) != 0)
    return FALSE;
  wrote = bfd_bwrite (ARMAG, SARMAG, arch);
  if (wrote != SARMAG)
    return FALSE;

  if (makemap && hasobjects)
    {
      if (! _bfd_compute_and_write_armap (arch, (unsigned int)0))
        return FALSE;
    }

  for (current = arch->archive_head;
       current != NULL;
       current = current->archive_next)
    {
      char buffer[DEFAULT_BUFFERSIZE];
      unsigned int remaining = arelt_size (current);
      
      /* the format of SX coff files is different from general coff (it uses 
         larger fields for storing filenames and section sizes) */
      {
        sx_coff_ar_file_hdr *sx_hdr = coff_sx_arch_hdr(current);
        /* Write ar header.  */
        if (bfd_bwrite (sx_hdr, sizeof (*sx_hdr), arch)
            != sizeof (*sx_hdr))
          return FALSE;
      }
      if (bfd_seek (current, (file_ptr) 0, SEEK_SET) != 0)
        goto input_err;
      while (remaining)
        {
          unsigned int amt = DEFAULT_BUFFERSIZE;
          if (amt > remaining)
            amt = remaining;
          errno = 0;
          if (bfd_bread (buffer, amt, current) != amt)
            {
              if (bfd_get_error () != bfd_error_system_call)
                bfd_set_error (bfd_error_file_truncated);
              goto input_err;
            }
          if (bfd_bwrite (buffer, amt, arch) != amt)
            return FALSE;
          remaining -= amt;
        }
      if ((arelt_size (current) % 2) == 1)
        {
          if (bfd_bwrite ("\012", 1, arch) != 1)
            return FALSE;
        }
    }
  
  if (makemap && hasobjects)
    {
      /* Verify the timestamp in the archive file.  If it would not be
         accepted by the linker, rewrite it until it would be.  If
         anything odd happens, break out and just return.  (The
         Berkeley linker checks the timestamp and refuses to read the
         table-of-contents if it is >60 seconds less than the file's
         modified-time.  That painful hack requires this painful hack.  */
      tries = 1;
      do
        {
          if (bfd_update_armap_timestamp (arch))
            break;
          (*_bfd_error_handler)
            (_("Warning: writing archive was slow: rewriting timestamp\n"));
        }
      while (++tries < 6);
    }
  
  return TRUE;

 input_err:
  bfd_set_error (bfd_error_on_input, current, bfd_get_error ());
  return FALSE;
}

/* these generic funcs should do for SX COFF as well (tested) */
#define coff_sx_openr_next_archived_file \
  _bfd_archive_coff_openr_next_archived_file
#define coff_sx_get_elt_at_index         \
  _bfd_archive_coff_get_elt_at_index
#define coff_sx_update_armap_timestamp   \
  _bfd_archive_coff_update_armap_timestamp


/* Called to copy BFD general private data from one object file
   to another.  */
static bfd_boolean 
coff_sx_bfd_copy_private_bfd_data(bfd *ibfd, bfd *obfd)
{
  /* copy SX-specific attributes of file header. */
  coff_data(obfd)->f_flags2 = coff_data(ibfd)->f_flags2;
  coff_data(obfd)->f_flags3 = coff_data(ibfd)->f_flags3;
  coff_data(obfd)->f_flttype = coff_data(ibfd)->f_flttype;
  
  /* copy Super-UX "optional" a.out Header. */
  coff_data(obfd)->aout_magic = coff_data(ibfd)->aout_magic;
  coff_data(obfd)->ssize = coff_data(ibfd)->ssize;
  
  return TRUE;
}

static bfd_boolean 
coff_sx_bfd_merge_private_bfd_data (bfd *ibfd, bfd *obfd)
{    
  /* FIXME: currently we are doing a bitwise OR of all the flags we encounter 
     in any of the objects file. This process produces flags which are 
     different from original SX compiler. Check if this is ok... */
  coff_data(obfd)->f_flags2 |= coff_data(ibfd)->f_flags2;
  
  if (coff_data(ibfd)->f_flags3 & F3_P64)
    {
      /* if the F3_P64 flag was set in one of the object files, set it also
         in the header of the executable. */
      coff_data(ibfd)->f_flags3 |= F3_P64; 
    }
    
  if (coff_data(obfd)->f_flttype != FT_NON &&
      coff_data(ibfd)->f_flttype != FT_NON && 
      coff_data(obfd)->f_flttype != coff_data(ibfd)->f_flttype)
    {
      /* if we encountered two different floating point data formats, we have to
         switch to mixed floating type mode (float0/float1/float2) */
      coff_data(obfd)->f_flttype = FT_MIX;
    }
  else if (coff_data(obfd)->f_flttype == FT_NON)
    {
      /* if we haven't set any floating point data format so far, assign
         the current objec files's floating point data format to the 
         executable. */
      coff_data(obfd)->f_flttype = coff_data(ibfd)->f_flttype;
    }
      
  return TRUE;
}

/* for the rest of the copy methods, we use the one from general COFF */
#define coff_sx_bfd_copy_private_section_data coff_bfd_copy_private_section_data
#define coff_sx_bfd_copy_private_symbol_data  coff_bfd_copy_private_symbol_data
#define coff_sx_bfd_copy_private_header_data  coff_bfd_copy_private_header_data
#define coff_sx_bfd_set_private_flags         coff_bfd_set_private_flags
#define coff_sx_bfd_print_private_bfd_data    coff_bfd_print_private_bfd_data

static reloc_howto_type howto_table[] = {
#define R_SX_ABS_IDX 0
  /* reloc type 0 (absolute) is ignored. reloc reading code
     ensures that this is a reference to the .abs section,
     which will cause bfd_perform_relocation to do nothing. */
  HOWTO(R_SX_ABS,               /* type */
	0,                      /* rightshift */
	0,                      /* size */
	8,                      /* bit size */
	FALSE,                  /* PC-relative? */
	0,                      /* bit position */
	complain_overflow_dont, /* nothing on overflow */
	NULL,                   /* special function */
	"ABSOLUTE",             /* name */
	FALSE,                  /* partial in-place? */
	0,                      /* source mask */
	0,                      /* destination mask */
	0),                     /* PC-relative offset */
  /* word size (32-bit) reference to symbol vaddr */
#define R_SX_RELLONG_IDX 1
  HOWTO(R_SX_RELLONG,
	0,                      /* rightshift */
	2,                      /* size */
	32,                     /* bit size */
	FALSE,                  /* PC-relative? */
	0,                      /* bit position */
	complain_overflow_bitfield, /* nothing on overflow */
	NULL,                   /* special function */
	"RELLONG",              /* name */
	TRUE,                   /* partial in-place? */
	0xFFFFFFFF,             /* source mask */
	0xFFFFFFFF,             /* destination mask */
	0),                     /* PC-relative offset */
  /* long word size (64-bit) reference to symbol vaddr */
#define R_SX_RELLLNG_IDX 2
  HOWTO(R_SX_RELLLNG,
	0,                      /* rightshift */
	4,                      /* size */
	64,                     /* bit size */
	FALSE,                  /* PC-relative? */
	0,                      /* bit position */
	complain_overflow_dont, /* nothing on overflow */
	NULL,                   /* special function */
	"RELLLNG",               /* name */
	TRUE,                   /* partial in-place? */
	0xFFFFFFFFFFFFFFFFLL,   /* source mask */
	0xFFFFFFFFFFFFFFFFLL,   /* destination mask */
	0),                     /* PC-relative offset */
  /* procedural address reference (64-bit) to symbol vaddr */
#define R_SX_RELPROC_IDX 3
  HOWTO(R_SX_RELPROC,
	0,                      /* rightshift */
	4,                      /* size */
	64,                     /* bit size */
	FALSE,                  /* PC-relative? */
	0,                      /* bit position */
	complain_overflow_dont, /* nothing on overflow */
	NULL,                   /* special function */
	"RELPROC",              /* name */
	TRUE,                   /* partial in-place? */
	0xFFFFFFFFFFFFFFFFLL,   /* source mask */
	0xFFFFFFFFFFFFFFFFLL,   /* destination mask */
	0),                     /* PC-relative offset */
  /* long word size (64-bit) reference to local common symbol vaddr */
#define R_SX_RELLCOM_IDX 4
  HOWTO(R_SX_RELLCOM,
	0,                      /* rightshift */
	4,                      /* size */
	64,                     /* bit size */
	FALSE,                  /* PC-relative? */
	0,                      /* bit position */
	complain_overflow_dont, /* nothing on overflow */
	NULL,                   /* special function */
	"RELLCOM",              /* name */
	TRUE,                   /* partial in-place? */
	0xFFFFFFFFFFFFFFFFLL,   /* source mask */
	0xFFFFFFFFFFFFFFFFLL,   /* destination mask */
	0),                     /* PC-relative offset */
};

#define coff_rtype_to_howto         coff_sx_rtype_to_howto

static reloc_howto_type *
coff_sx_rtype_to_howto (bfd *abfd ATTRIBUTE_UNUSED,
                        asection *sec ATTRIBUTE_UNUSED,
                        struct internal_reloc *rel,
                        struct coff_link_hash_entry *h ATTRIBUTE_UNUSED,
                        struct internal_syment *sym ATTRIBUTE_UNUSED,
                        bfd_vma *addendp ATTRIBUTE_UNUSED)
{
  reloc_howto_type *howto;

  if (rel->r_type == R_SX_ABS) 
    {
      /* R_ABS has a value of 0 */
      howto = howto_table; 
    }
  else if (rel->r_type >= R_SX_RELLONG && rel->r_type <= R_SX_RELLCOM)
    {
      /* according SX COFF specifications, all of SX relocation types (except 
         for R_ABS) are between R_SX_RELLONG (i.e. 021) and R_SX_RELLCOM 
         (i.e. 024) */
      howto = howto_table + (R_SX_RELLONG_IDX + (rel->r_type - R_SX_RELLONG));
    }
  else
    {
      /* undefined relocation type*/
      bfd_set_error(bfd_error_bad_value);
      return NULL;
    }

  return howto;
}

#define RTYPE2HOWTO(cache_ptr, dst)  coff_sx_rtype2howto(cache_ptr, dst)

static void 
coff_sx_rtype2howto(arelent *cache_ptr, struct internal_reloc *dst)
{
  if (dst->r_type == R_SX_ABS) 
    {
      /* R_ABS has a value of 0 */
      cache_ptr->howto = howto_table; 
    }
  else if (dst->r_type >= R_SX_RELLONG && dst->r_type <= R_SX_RELLCOM)
    {
      /* according SX COFF specifications, all of SX relocation types (except 
         for R_ABS) are between R_SX_RELLONG (i.e. 021) and R_SX_RELLCOM 
         (i.e. 024) */
      cache_ptr->howto = howto_table + (R_SX_RELLONG_IDX +
					(dst->r_type - R_SX_RELLONG));
    }
  else
    {
      /* undefined relocation type*/
      cache_ptr->howto = NULL;
    }
}

#define coff_bfd_reloc_type_lookup  coff_sx_reloc_type_lookup

static reloc_howto_type *
coff_sx_reloc_type_lookup(bfd *abfd ATTRIBUTE_UNUSED,
			  bfd_reloc_code_real_type code)
{
  switch(code)
    {
    case BFD_RELOC_SX_ABS:
      return &(howto_table[R_SX_ABS_IDX]);
    case BFD_RELOC_SX_RELLONG:
      return &(howto_table[R_SX_RELLONG_IDX]);
    case BFD_RELOC_SX_RELLLNG:
      return &(howto_table[R_SX_RELLLNG_IDX]);
    case BFD_RELOC_SX_RELPROC:
      return &(howto_table[R_SX_RELPROC_IDX]);
    case BFD_RELOC_SX_RELLCOM:
      return &(howto_table[R_SX_RELLCOM_IDX]);
    default:
      BFD_FAIL();
      return NULL;
    }
}

#define coff_bfd_reloc_name_lookup  coff_sx_reloc_name_lookup

static reloc_howto_type *
coff_sx_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			   const char *r_name)
{
  unsigned int i;

  for (i = 0; i < sizeof (howto_table) / sizeof (howto_table[0]); i++)
    if (howto_table[i].name != NULL
	&& strcasecmp (howto_table[i].name, r_name) == 0)
      return &howto_table[i];

  return NULL;
}

#ifndef bfd_pe_print_pdata
#  define bfd_pe_print_pdata  NULL
#endif

#define coff_relocate_section _bfd_coff_generic_relocate_section

/* now that we have everything required defined and declared:
   include coffcode.h */
#include "coffcode.h"

const bfd_target TARGET_BIG_SYM =
{
  TARGET_BIG_NAME,
  bfd_target_coff_flavour,
  BFD_ENDIAN_BIG,		/* Data byte order is big.  */
  BFD_ENDIAN_BIG,		/* Header byte order is big.  */

  (HAS_RELOC | EXEC_P |		/* Object flags.  */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC |
   SEC_CODE | SEC_DATA),
  0,				/* Leading char.  */
  '/',        /* AR_pad_char.  */
  64,				/* AR_max_namelen.  */

  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
  bfd_getb32, bfd_getb_signed_32, bfd_putb32,
  bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* data */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
  bfd_getb32, bfd_getb_signed_32, bfd_putb32,
  bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* hdrs */

  /* Note that we allow an object file to be treated as a core file as well.  */
  {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
   coff_sx_archive_p, coff_object_p},
  {bfd_false, coff_mkobject, _bfd_generic_mkarchive, /* bfd_set_format */
   bfd_false},
  {bfd_false, coff_write_object_contents, /* bfd_write_contents */
   coff_sx_write_archive_contents, bfd_false},

  BFD_JUMP_TABLE_GENERIC (coff),
  BFD_JUMP_TABLE_COPY (coff_sx),
  BFD_JUMP_TABLE_CORE (_bfd_nocore),
  BFD_JUMP_TABLE_ARCHIVE (coff_sx),
  BFD_JUMP_TABLE_SYMBOLS (coff),
  BFD_JUMP_TABLE_RELOCS (coff),
  BFD_JUMP_TABLE_WRITE (coff),
  BFD_JUMP_TABLE_LINK (coff),
  BFD_JUMP_TABLE_DYNAMIC (_bfd_nodynamic),

  NULL,

  COFF_SWAP_TABLE
};

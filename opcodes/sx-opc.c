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
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, U\SA.

   Authors: Erich Focht <efocht@hpce.nec.com>
            jaKa Mocnik <jaka@xlab.si>
            Marko Novak <marko.novak@xlab.si>
            Matthias Hess
*/


#include "opcode/sx.h"

#define _NA            { 0  , 0  , 0  , 0 }
#define _X(x)          { (x), 0  , 0  , 0 }
#define _Y(y)          { 0  , (y), 0  , 0 }
#define _Z(z)          { 0  , 0  , (z), 0 }
#define _XY(x,y)       { (x), (y), 0  , 0 }
#define _XZ(x,z)       { (x), 0  , (z), 0 }
#define _YZ(y,z)       { 0  , (y), (z), 0 }
#define _XYZ(x,y,z)    { (x), (y), (z), 0 }
#define _YZD(y,z,d)    { 0  , (y), (z), (d) }
#define _ZD(z,d)       { 0  , 0  , (z), (d) }
#define _XYZD(x,y,z,d) { (x), (y), (z), (d) }

#define _OPC(a,b,c,d,e)   { a, b, c, d, e, { 0, 0, 0 } }
#define _OPV(a,b,c,d,e,f) { a, b, c, d, e, f }
#define _VREG(a, b, c)    { a, b, c }

/*
 * SX opcode table
 *
 * The _OPC() macro defines opcodes which don't depend on fixed vector registers.
 * _OPV() defines opcodes which have fixed vector registers.
 *
 * Table entries have the format:
 *
 * mnemonic, opcode, format, extended format,
 *        { x field, y field, z, field, d field },
 *        { xvreg, yvreg, zvreg }
 *
 */


const struct sx_opcode sx_opcodes[] = {
    _OPC("dummy", 0x00, 0    , 0      , _NA),
    /* for RX fmt instructions (with ASX), registers are as follows:
       - X: destination sreg
       - Y: index (sreg or signed 7bit immediate)
       - Z: base (sreg or zero)
       - D: displacement
       thus: _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)
    */
    _OPC("lds"  , 0x01, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ldu"  , 0x02, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ldl"  , 0x03, F_RX+FM_ASX, FS_ZEROH  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ld2b" , 0x04, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ld1b" , 0x05, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("lea"  , 0x06, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    /* TODO: what is tla? not documented anywhere; besides, it should have ASX fmt or
       different field checks ... */
    _OPC("tla"  , 0x07, F_RX       , 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bsic" , 0x08, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("dlds" , 0x09, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("dldu" , 0x0a, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("dldl" , 0x0b, F_RX+FM_ASX, FS_ZEROH  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("dummy", 0x0c, 0          , 0  , _NA),
    _OPC("dummy", 0x0d, 0          , 0  , _NA),
    /* LDM and STM are only RZ instructions:
       - X: destination (source for STM) sreg
       - Y: register count (unsigned 7bit immediate)
       - Z: base (sreg or zero)
       - D: displacement
    */
    _OPC("ldm"  , 0x0e, F_RZ+FM_AS , 0  , _XYZD(TSREG, TLI127, TSREG|TLZERO, TDISPL)),
    _OPC("cvd"  , 0x0f, F_RR       , 0  , _XY(TSREG, TSREG|TLIS63)),
    _OPC("dummy", 0x10, 0          , 0  , _NA),
    _OPC("sts"  , 0x11, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("stu"  , 0x12, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("stl"  , 0x13, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("st2b" , 0x14, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("st1b" , 0x15, F_RX+FM_ASX, 0  , _XYZD(TSREG, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("dummy", 0x16, 0          , 0  , _NA),
    _OPC("dummy", 0x17, 0          , 0  , _NA),
    _OPC("dummy", 0x18, 0          , 0  , _NA),
    //_OPC("bc" , 0x19, F_CFX+FM_AS, 0  , _XYZD(TCONDF, TLIS63, TLZERO, TDISPL)),
    /* CFX fmt instructions ((conditional) branches) use
       - X: conditional flags (explicit or implied via mnemonic)
       - Y: val to test (sreg or signed 7bit immediate)
       - Z: base (sreg or zero)
       - D: displacement
       TODO: we should add an extra field to opcode struct that would fill in condf in
       X field in order to avoid big strcmp chains in assembly code */
    _OPC("bc"   , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _XYZD(TCONDF, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bh"   , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bl"   , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bne"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("be"   , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhe"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ble"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bcneqn",0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bceqn", 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhn"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bln"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bnen" , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("ben"  , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhen" , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blen" , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("b"    , 0x19, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _ZD(TSREG|TLZERO, TDISPL)),
    _OPC("dummy", 0x1a, 0          , 0  , _NA),
    //_OPC("bcs", 0x1b, F_CFX+FM_AS, 0  , _XYZD(TCONDF, TSREG, TLZERO, TDISPL)),
    _OPC("bcs"  ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _XYZD(TCONDF, TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhs"  ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bls"  ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bnes" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bes"  ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhes" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bles" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bcsneqn", 0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bcseqn",  0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhsn" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blsn" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bnesn",   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("besn" ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhesn",   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blesn",   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bs"   ,   0x1b, F_CFX+FM_AS, FS_BPYES|FS_BPNO  , _ZD(TSREG|TLZERO, TDISPL)),
    _OPC("bcf"  ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _XYZD(TCONDF, TSREG|TLIS63, TLZERO, TDISPL)),
    _OPC("bhf"  ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blf"  ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bnef" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bef"  ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhef" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blef" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bcfneqn", 0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bcfeqn",  0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhfn" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blfn" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bnefn",   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("befn" ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bhefn",   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("blefn",   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO|FS_SPOP, _YZD(TSREG|TLIS63, TSREG|TLZERO, TDISPL)),
    _OPC("bf"   ,   0x1c, F_CFX+FM_AS, FS_BPYES|FS_BPNO      , _ZD(TSREG|TLZERO, TDISPL)),
    _OPC("dummy", 0x1d, 0            , 0      , _NA),
    _OPC("stm"  , 0x1e, F_RZ+FM_AS, 0         , _XYZD(TSREG, TLI127, TSREG|TLZERO, TDISPL)),
    _OPC("cvs"  , 0x1f, F_RR      , 0         , _XY(TSREG, TSREG|TLIS63)),
    _OPC("lcr"  , 0x20, F_RR      , FS_EXTOP  , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("las"  , 0x21, F_RR      , 0         , _XZ(TSREG, TSREG|TLM0M1)),
    //                                                              offset
    _OPC("stvecc",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),          //0x00
    _OPC("stfpec",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x01
    _OPC("stocmcc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x02
    _OPC("stbccc",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x03
    _OPC("stdidr",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x04
    _OPC("stsar",   0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x05
    _OPC("sticmcc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x06
    _OPC("stsidr",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x07
    _OPC("stbpfc",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x08
    _OPC("stusrcc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x09
    _OPC("stiphcc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0a
    /* TODO: stspec is SX-4 only - we need to denote it in order to report
       it as error on SX-5+ */
    _OPC("stspec",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0b
    _OPC("stvarec", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0c
    _OPC("stvldec", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0d
    /* TODO: stmnubc is not documented anywhere */
    _OPC("stmnubc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0e
    _OPC("stsracc", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x0f
    _OPC("stmidr0", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x10
    _OPC("stmidr1", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x11
    _OPC("stmidr2", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x12
    _OPC("stmidr3", 0x22, F_RR , FS_EXTOP    , _X(TSREG)),	    //0x13
    _OPC("smir" ,   0x22, F_RR , FS_EXTOP    , _XY(TSREG, TSOFT)),   //0x14
    _OPC("smir" ,   0x22, F_RR , FS_EXTOP    , _XY(TSREG, TSOFT)),   //0x15
    _OPC("smir" ,   0x22, F_RR , FS_EXTOP    , _XY(TSREG, TSOFT)),   //0x16
    _OPC("smir" ,   0x22, F_RR , FS_EXTOP    , _XY(TSREG, TSOFT)),   //0x17
    /* TODO: stpmmr is not documented anywhere ... */
    _OPC("stpmmr",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),	     //0x18
    _OPC("stjidr",  0x22, F_RR , FS_EXTOP    , _X(TSREG)),           //0x19
    /* TODO: sthrr is not documented anywhere ... */
    _OPC("sthrr" ,  0x22, F_RR , FS_EXTOP    , _XY(TSREG, TSOFT)),   //0x1a
    _OPC("lal"  , 0x23, F_RR , 0        , _XZ(TSREG, TSREG|TLM0M1)),
    _OPC("svt"  , 0x24, F_RR , 0        , _X(TSREG)),
    _OPC("sex"  , 0x25, F_RR , 0        , _X(TSREG)),
    _OPC("svx"  , 0x26, F_RR , 0        , _X(TSREG)),
    _OPC("sve"  , 0x27, F_RR , 0        , _X(TSREG)),
    _OPC("sic"  , 0x28, F_RR , 0        , _X(TSREG)),
    _OPC("sfr"  , 0x29, F_RR , 0        , _X(TSREG)),
    _OPC("spm"  , 0x2a, F_RR , 0        , _X(TSREG)),
    _OPC("ssm"  , 0x2b, F_RR , 0        , _X(TSREG)),
    _OPC("spsw" , 0x2c, F_RR , 0        , _X(TSREG)),
    _OPC("rpn"  , 0x2d, F_RR , 0        , _X(TSREG)),
    _OPC("smvl" , 0x2e, F_RR , 0        , _X(TSREG)),
    _OPC("svl"  , 0x2f, F_RR , 0        , _X(TSREG)),
    _OPC("scr"  , 0x30, F_RR , FS_EXTOP , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("sas"  , 0x31, F_RR , 0        , _XZ(TSREG, TSREG|TLM0M1)),
    //_OPC("lmir" , 0x32, F_RR , 0      , _Z(TLM0M1)), //------offset------
    _OPC("ldvecc",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),         //0x00
    _OPC("ldfpec",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x01
    _OPC("ldocmcc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x02
    _OPC("ldbccc",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x03
    _OPC("lddidr",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x04
    _OPC("ldsar",   0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x05
    _OPC("ldicmcc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x06
    _OPC("ldsidr",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x07
    _OPC("ldbpfc",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x08
    _OPC("ldusrcc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x09
    _OPC("ldiphcc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0a
    _OPC("ldspec",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0b
    _OPC("ldvarec", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0c
    _OPC("ldvldec", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0d
    _OPC("ldmnubc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0e
    _OPC("ldsracc", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x0f
    _OPC("ldmidr0", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x10
    _OPC("ldmidr1", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x11
    _OPC("ldmidr2", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x12
    _OPC("ldmidr3", 0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),	  //0x13
    /* TODO: why do we repeat lmir 4 times here?! */
    _OPC("lmir" ,   0x32, F_RR , FS_EXTOP  , _YZ(TSOFT,TSREG|TLM0M1)),  //0x14
    _OPC("lmir" ,   0x32, F_RR , FS_EXTOP  , _YZ(TSOFT,TSREG|TLM0M1)),  //0x15
    _OPC("lmir" ,   0x32, F_RR , FS_EXTOP  , _YZ(TSOFT,TSREG|TLM0M1)),  //0x16
    _OPC("lmir" ,   0x32, F_RR , FS_EXTOP  , _YZ(TSOFT,TSREG|TLM0M1)),  //0x17
    /* TODO: what is ldpmmr? not documented anywhere ... */
    _OPC("ldpmmr",  0x32, F_RR , FS_EXTOP  , _Z(TLM0M1)),	  //0x18
    /* originally, ldjidr was _YZ(TSOFT,TSREG|TLM0M1); docs say, it's only _Z(TSREG|TLM0M1) */
    _OPC("ldjidr",  0x32, F_RR , FS_EXTOP  , _Z(TSREG|TLM0M1)),  //0x19
    /* TODO: what is ldhrr? not documented anywhere ... */
    _OPC("ldhrr" ,  0x32, F_RR , FS_EXTOP  , _YZ(TSOFT,TSREG|TLM0M1)),  //0x1a
    _OPC("sal"  , 0x33, F_RR , 0           , _XZ(TSREG, TSREG|TLM0M1)),
    _OPC("ldt"  , 0x34, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("lex"  , 0x35, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("lvx"  , 0x36, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("lve"  , 0x37, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("inh"  , 0x38, F_RR , 0           , _NA),
    _OPC("eni"  , 0x39, F_RR , 0           , _NA),
    _OPC("lpm"  , 0x3a, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("lsm"  , 0x3b, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("rcr"  , 0x3c, F_RR , 0           , _Y(TLI127)),
    _OPC("diag" , 0x3d, F_RR , 0           , _Y(TLI127)),
    _OPC("wait" , 0x3e, F_RR , 0           , _NA),
    _OPC("monc" , 0x3f, F_RR , 0           , _XYZ(TSOFT, TSOFT, TSOFT)),
    _OPC("tscr" , 0x40, F_RR , FS_EXTOP    , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("scrns", 0x41, F_RR , FS_EXTOP    , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("ts1am", 0x42, F_RR , 0           , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("ts2am", 0x43, F_RR , 0           , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("and"  , 0x44, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("or"   , 0x45, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("xor"  , 0x46, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("eqv"  , 0x47, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("add"  , 0x48, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("mpy"  , 0x49, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("ads"  , 0x4a, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("mps"  , 0x4b, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fad"  , 0x4c, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fmp"  , 0x4d, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fix"  , 0x4e, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TOPT|TSOFT)),
    _OPC("fixx" , 0x4f, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TOPT|TSOFT)),
    _OPC("ldcl" , 0x50, F_RR , 0           , _XZ(TSREG, TSREG)),
    /* TODO: scm is not documented anywhere ... */
    _OPC("scm"  , 0x51, F_RR , 0           , _XYZ(TSREG, TLIS63, TLM0M1)),
    _OPC("ts3am", 0x52, F_RR , 0           , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("atmam", 0x53, F_RR , 0           , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("nnd"  , 0x54, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("cmp"  , 0x55, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("mrg"  , 0x56, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("slax" , 0x57, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("sub"  , 0x58, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("adx"  , 0x59, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("sbs"  , 0x5a, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("sbx"  , 0x5b, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fsb"  , 0x5c, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fdv"  , 0x5d, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("flt"  , 0x5e, F_RR , FS_SPOP     , _XY(TSREG, TSREG|TLIS63)),
    _OPC("fltx" , 0x5f, F_RR , 0           , _XY(TSREG, TSREG|TLIS63)),
    /* _OPC("fidcr", 0x60, F_RR , FS_EXTOP    , _XYZ(TSREG, TSREG|TLI127, TLIS63)), */
    _OPC("fidcr", 0x60, F_RR , FS_EXTOP    , _XYZ(TSREG, TSREG|TLI127, TSOFT)),
    _OPC("snpsb", 0x61, F_RR , 0           , _X(TSREG)),
    _OPC("scpsb", 0x62, F_RR , 0           , _X(TSREG)),
    _OPC("dummy", 0x63, 0    , 0           , _NA),
    _OPC("sld"  , 0x64, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("sll"  , 0x65, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("sla"  , 0x66, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("ldz"  , 0x67, F_RR , 0           , _XZ(TSREG, TSREG|TLM0M1)),
    _OPC("rmsg" , 0x68, F_RR , 0           , _X(TSREG)),
    _OPC("lfr"  , 0x69, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("cpx"  , 0x6a, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    /* _OPC("ssr"  , 0x6b, F_RR , 0           , _XY(TSREG, TLI127)), */
    _OPC("ssr"  , 0x6b, F_RR , 0           , _XZ(TSREG, TSOFT)),
    _OPC("ststm", 0x6b, F_RR , 0           , _X(TSREG)),
    _OPC("stitm", 0x6b, F_RR , 0           , _X(TSREG)),
    /* TODO: stgtm not documented anywhere */
    _OPC("stgtm", 0x6b, F_RR , 0           , _X(TSREG)),
    _OPC("stgtm", 0x6b, F_RR , 0           , _X(TSREG)),
    _OPC("faq"  , 0x6c, F_RW , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fmq"  , 0x6d, F_RW , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("mpx"  , 0x6e, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("dummy", 0x6f, 0    , 0           , _NA),
    _OPC("ret"  , 0x70, F_RR , 0           , _Y(TSOFT|TOPT)),
    _OPC("lnpsb", 0x71, F_RR , 0           , _Z(TSREG)),
    _OPC("lcpsb", 0x72, F_RR , 0           , _Z(TSREG)),
    _OPC("chgsp", 0x73, F_RR , FS_EXTOP    , _YZ(TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("srd"  , 0x74, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("srl"  , 0x75, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("sra"  , 0x76, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("srax" , 0x77, F_RR+FM_XZY, 0     , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    /* TODO: the HTML manual says field Z, paper ref says field X - whom do we believe? ;) */
    _OPC("send" , 0x78, F_RR , FS_EXTOP    , _Z(TSREG)),
    _OPC("nop"  , 0x79, F_RR , 0           , _NA),
    _OPC("cps"  , 0x7a, F_RR , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("lsr"  , 0x7b, F_RR , 0           , _YZ(TSREG|TLI127, TSOFT)),
    _OPC("ldstm", 0x7b, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("lditm", 0x7b, F_RR , 0           , _Y(TSREG|TLI127)),
    _OPC("fsq"  , 0x7c, F_RW , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fcq"  , 0x7d, F_RW , 0           , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("fcp"  , 0x7e, F_RR , FS_SPOP     , _XYZ(TSREG, TSREG|TLIS63, TSREG|TLM0M1)),
    _OPC("dummy", 0x7f, 0    , 0           , _NA),

    /* vector stuff begins here ... */

    _OPC("vldx" , 0x80, F_RRX+FM_DXZ_Y, 0                         , _XYZD(TVDVC|TVREG, TSREG|TSOFT, TSREG|TLM0M1, TVDZC|TVDREG)),
    _OPC("vld"  , 0x81, F_RR          , 0                         , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("vldu" , 0x82, F_RR          , 0                         , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("vldl" , 0x83, F_RR          , FS_ZEROH                  , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("andm" , 0x84, F_RR          , 0                         , _XYZ(TVMREG, TVMREG, TVMREG)),
    _OPC("orm"  , 0x85, F_RR          , 0                         , _XYZ(TVMREG, TVMREG, TVMREG)),
    _OPC("xorm" , 0x86, F_RR          , 0                         , _XYZ(TVMREG, TVMREG, TVMREG)),
    _OPC("eqvm" , 0x87, F_RR          , 0                         , _XYZ(TVMREG, TVMREG, TVMREG)),
    _OPC("mvcm" , 0x88, F_RR          , 0                         , _X(TVMREG)),
    _OPV("vmaxs", 0x89, F_RV          , FS_YFIX+FS_SELVU+FS_SELEL , _XY(TVREG, TVREG), _VREG(0, VA0, 0)),
    _OPC("lvm"  , 0x8a, F_RR          , 0                         , _XYZ(TVMREG, TSREG|TLI127, TSOFT|TOPT)),
    _OPV("vadx" , 0x8b, F_RV+FM_DXZ_Y , FS_ZFIX+FS_MSKOP+FS_SELVU , _XYZ(TVDVC|TVREG, TSREG|TLI127|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPC("vbrd" , 0x8c, F_RV+FM_DXZ_Y , FS_MSKOP+FS_SELVU         , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG)),
    _OPC("vcp"  , 0x8d, F_RR          , 0                         , _XZ(TVREG, TVREG)),
    _OPC("lsv"  , 0x8e, F_RR+FM_DXY_Z , 0                         , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("vcvd" , 0x8f, F_RR+FM_DXZ_Y , FS_MSKOP+FS_SELVU         , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG)),
    _OPC("vldux", 0x90, F_RRX+FM_DXZ_Y, 0                         , _XYZD(TVDVC|TVREG, TSREG|TSOFT, TSREG|TLM0M1, TVDZC|TVDREG)),
    _OPC("vst"  , 0x91, F_RR          , FS_MSKOP                  , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("vstu" , 0x92, F_RR , FS_MSKOP                           , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("vstl" , 0x93, F_RR , FS_MSKOP                           , _XYZ(TVREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("nndm" , 0x94, F_RR , 0                                  , _XYZ(TVMREG, TVMREG, TVMREG)),
    _OPC("negm" , 0x95, F_RR , 0                                  , _XY(TVMREG, TVMREG)),
    _OPC("dummy", 0x96, 0    , 0                                  , _NA),
    _OPC("dummy", 0x97, 0    , 0                                  , _NA),
    _OPC("mvmc" , 0x98, F_RR , 0                                  , _Y(TVMREG)),
    _OPV("vmins", 0x99, F_RV , FS_YFIX+FS_SELVU+FS_SELEL          , _XY(TVREG, TVREG), _VREG(0, VA0, 0)),
    _OPC("svm"  , 0x9a, F_RR , 0                                  , _XYZ(TSREG, TVMREG, TSOFT|TOPT)),
    _OPV("vsbx" , 0x9b, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPC("vmv"  , 0x9c, F_RR , FS_MSKOP                           , _XYZ(TVREG, TSREG|TLI127, TVREG)),
    _OPC("vex"  , 0x9d, F_RR , 0                                  , _XZ(TVREG, TVREG)),
    _OPC("lvs"  , 0x9e, F_RR+FM_DX_ZY, 0                          , _XYZ(TSREG, TSREG|TLI127, TVREG)),
    _OPC("vcvs" , 0x9f, F_RR+FM_DXZ_Y, FS_MSKOP+FS_SELVU          , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG)),
    _OPC("vldlx", 0xa0, F_RRX+FM_DXZ_Y, FS_ZEROH                  , _XYZD(TVDVC|TVREG, TSREG|TSOFT, TSREG|TLM0M1, TVDZC|TVDREG)),
    _OPC("vgt"  , 0xa1, F_RR+FM_DXZ_Y , FS_MSKOP                  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG)),
    _OPC("vgtu" , 0xa2, F_RR+FM_DXZ_Y, FS_MSKOP                   , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG)),
    _OPC("vgtl" , 0xa3, F_RR+FM_DXZ_Y, FS_MSKOP+FS_ZEROH          , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG)),
    _OPC("dummy", 0xa4, 0    , 0                                  , _NA),
    _OPC("tovc" , 0xa5, F_RR , 0                                  , _X(TSREG)),
    _OPC("pcnt" , 0xa6, F_RR , 0                                  , _X(TSREG)),
    _OPC("lzvc" , 0xa7, F_RR , 0                                  , _X(TSREG)),
    /* TODO: not sure about vfixx & vfsx checks - need to doublecheck with manual*/
    _OPV("vfixx", 0xa8, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG|TSOFT|TOPT),	  _VREG(0, VA0, 0)),
    _OPV("vfsx" , 0xa9, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	  _VREG(0, VA0, 0)),
    _OPV("vsumx", 0xaa, F_RV , FS_YFIX+FS_ZFIX+FS_SELVU           , _XYZ(TVREG, TVREG, TVREG),			  _VREG(0, VA0, VA1)),
    _OPV("vmaxx", 0xab, F_RV , FS_YFIX+FS_SELVU+FS_SELEL          , _XY(TVREG, TVREG),			  _VREG(0, VA0, 0)),
    _OPC("dummy", 0xac, 0    , 0                                  , _NA),
    _OPV("vfmax", 0xad, F_RV , FS_YFIX+FS_SELVU+FS_SELEL+FS_SPOP  , _XY(TVREG, TVREG),			  _VREG(0, VA0, 0)),
    _OPC("vmad" , 0xae, F_RR+FM_ZX, FS_MSKOP                      , _XZ(TVREG, TVDREG)),
    _OPC("dummy", 0xaf, 0    , 0                                  , _NA),
    _OPV("vsfa" , 0xb0, F_RRX+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU , _XYZD(TVDVC|TVREG, TSREG|TLIS63, TSREG|TLM0M1, TVDZC|TVDREG), _VREG(VL1, 0, 0)),
    _OPC("vsc"  , 0xb1, F_RR , FS_SELEL                           , _YZ(TVREG, TVREG)),
    _OPC("vscu" , 0xb2, F_RR , FS_VOVER|FS_MSKOP                           , _YZ(TVREG, TVREG)),
    _OPC("vscl" , 0xb3, F_RR , FS_VOVER|FS_MSKOP                           , _YZ(TVREG, TVREG)),
    _OPV("vfmk" , 0xb4, F_RR , FS_ZFIX+FS_MSKOP+FS_SELVU          , _XYZ(TVMREG, TCONDF, TVREG),              _VREG(0, 0, VL0)),
    _OPC("dummy", 0xb5, 0    , 0                                  , _NA),
    _OPV("vfms" , 0xb6, F_RR , FS_ZFIX+FS_MSKOP+FS_SELVU          , _XYZ(TVMREG, TCONDF, TVREG),		          _VREG(0, 0, VL0)),
    _OPV("vpcnt", 0xb7, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	          _VREG(0, VL0, 0)),
    _OPV("vfltx", 0xb8, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	          _VREG(0, VA0, 0)),
    _OPV("vcmp" , 0xb9, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),    _VREG(0, VA0, VA1)),
    _OPV("vcpx" , 0xba, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),    _VREG(0, VA0, VA1)),
    _OPV("vminx", 0xbb, F_RV , FS_YFIX+FS_SELVU+FS_SELEL          , _XY(TVREG, TVREG),				  _VREG(0, VA0, 0)),
    _OPV("vfmf" , 0xbc, F_RR , FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP  , _XYZ(TVMREG, TCONDF, TVREG),		          _VREG(0, 0, VL0)),
    _OPV("vfmin", 0xbd, F_RV , FS_YFIX+FS_SELVU+FS_SELEL+FS_SPOP  , _XY(TVREG, TVREG),				  _VREG(0, VA0, 0)),
    _OPC("vmda" , 0xbe, F_RR , 0                                  , _XZ(TVREG, TVDREG)),
    _OPC("lvl"  , 0xbf, F_RR , 0                                  , _Y(TSREG|TLI127)),
    _OPC("wxs"  , 0xc0, F_RR , 0                                  , _XY(TSREG, TSREG)),
    _OPC("wxm"  , 0xc1, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("awxm" , 0xc2, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("nwxm" , 0xc3, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPV("vand" , 0xc4, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLM0M1|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VL0, VL1)),
    _OPV("vor"  , 0xc5, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLM0M1|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VL0, VL1)),
    _OPV("vxor" , 0xc6, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLM0M1|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VL0, VL1)),
    _OPV("veqv" , 0xc7, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLM0M1|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VL0, VL1)),
    _OPV("vadd" , 0xc8, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VA0, VA1)),
    _OPV("vmpy" , 0xc9, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VP0, VP1)),
    _OPV("vads" , 0xca, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VA0, VA1)),
    _OPV("vmps" , 0xcb, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VP0, VP1)),
    _OPV("vfad" , 0xcc, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP   , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPV("vfmp" , 0xcd, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP   , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VP0, VP1)),
    _OPV("vfia" , 0xce, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG),			        _VREG(0, 0, VA0)),
    _OPV("vfim" , 0xcf, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG),			        _VREG(0, 0, VP1)),
    _OPC("rxs"  , 0xd0, F_RR , 0                                  , _X(TSREG)),
    _OPC("rxm"  , 0xd1, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("arxm" , 0xd2, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("nrxm" , 0xd3, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPV("vslax", 0xd4, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),          _VREG(VS0, 0, 0)),
    _OPV("vsrax", 0xd5, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),          _VREG(VS0, 0, 0)),
    _OPV("vmrg" , 0xd6, F_RV+FM_DXZ_Y, FS_ZFIX+FS_SELVU           , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VL0, VL1)),
    _OPC("dummy", 0xd7, 0    , 0                                  , _NA),
    _OPV("vsub" , 0xd8, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VA0, VA1)),
    _OPC("dummy", 0xd9, 0    , 0                                  , _NA),
    _OPV("vsbs" , 0xda, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VA0, VA1)),
    _OPV("vmpx", 0xdb, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG),          _VREG(0, VP0, VP1)),
    _OPV("vfsb" , 0xdc, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPV("vfdv" , 0xdd, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VS0, VS1)),
    _OPV("vfis" , 0xde, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG),			        _VREG(0, 0, VA0)),
    _OPC("dummy", 0xdf, 0    , 0                                  , _NA),
    _OPC("rrs"  , 0xe0, F_RR , 0                                  , _X(TSREG)),
    _OPC("rna"  , 0xe1, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("awxma", 0xe2, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("dummy", 0xe3, 0    , 0                                  , _NA),
    _OPV("vsld" , 0xe4, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),          _VREG(VS0VS1, 0, 0)),
    _OPV("vsll" , 0xe5, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),          _VREG(VS0, 0, 0)),
    _OPV("vsla" , 0xe6, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),          _VREG(VS0, 0, 0)),
    _OPC("dummy", 0xe7, 0    , 0                                  , _NA),
    /* TODO: not really sure vfix checks - doublecheck with manual */
    _OPV("vfix" , 0xe8, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG|TSOFT|TOPT),                   _VREG(0, VA0, 0)),
    _OPV("vfdb" , 0xe9, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	        _VREG(0, VS0, 0)),
    _OPV("vsum" , 0xea, F_RV , FS_YFIX+FS_ZFIX+FS_SELVU  , _XYZ(TVREG, TVREG, TVREG), _VREG(0, VA0, VA1)),
    _OPV("vsqr" , 0xeb, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),   _VREG(0, VP1, 0)),
    _OPV("vfsm" , 0xec, F_RV , FS_YFIX+FS_ZFIX+FS_SELVU+FS_SPOP   , _XYZ(TVREG, TVREG, TVREG),			        _VREG(0, VA0, VA1)),
    _OPV("vfsq" , 0xed, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	        _VREG(0, VP1, 0)),
    _OPV("viam" , 0xee, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG), _VREG(0, 0, VA0VP1)),
    _OPV("vima" , 0xef, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG), _VREG(0, 0, VA0VP1)),
    _OPC("dummy", 0xf0, F_RX , 0                                  , _NA),
    _OPC("arna" , 0xf1, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG|TLM0M1)),
    _OPC("arxma", 0xf2, F_RR , 0                                  , _XYZ(TSREG, TSREG|TLI127, TSREG)),
    _OPC("dummy", 0xf3, 0    , 0                                  , _NA),
    _OPV("vsrd" , 0xf4, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU, _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),           _VREG(VS0VS1, 0, 0)),
    _OPV("vsrl" , 0xf5, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU, _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),           _VREG(VS0, 0, 0)),
    _OPV("vsra" , 0xf6, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU, _XYZ(TVDVC|TVREG, TSREG|TLI127, TVDZC|TVDREG),           _VREG(VS0, 0, 0)),
    _OPV("vbrv" , 0xf7, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU, _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	                _VREG(0, VS0, 0)),
    _OPV("vflt" , 0xf8, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	        _VREG(0, VA0, 0)),
    _OPV("vfhf" , 0xf9, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG),	        _VREG(0, VS0, 0)),
    _OPV("vcps" , 0xfa, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPV("vsqrx", 0xfb, F_RV+FM_DXZ_Y, FS_YFIX+FS_MSKOP+FS_SELVU  , _XYZ(TVDVC|TVREG, TVREG, TVDZC|TVDREG), _VREG(0, VP1, 0)),
    _OPV("vfcp" , 0xfc, F_RV+FM_DXZ_Y, FS_ZFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TSREG|TLIS63|TVREG, TVDZC|TVDREG|TVREG), _VREG(0, VA0, VA1)),
    _OPV("vfdr" , 0xfd, F_RV+FM_DXZ_Y, FS_XFIX+FS_MSKOP+FS_SELVU+FS_SPOP, _XYZ(TVDVC|TVREG, TSREG|TLIS63, TVDZC|TVDREG), _VREG(VS0, 0, 0)),
    _OPV("vism" , 0xfe, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG), _VREG(0, 0, VA0VP1)),
    _OPV("vims" , 0xff, F_RV , FS_ZFIX+FS_SELVU+FS_SPOP           , _XYZ(TVREG, TSREG|TLIS63, TVREG), _VREG(0, 0, VA0VP1)),
};

const int sx_num_opcodes = sizeof (sx_opcodes) / sizeof (sx_opcodes[0]);

char *sx_fixed_vreg[] = {
    "",  
    "$v0",
    "$v1",
    "$v2",
    "$v3",
    "$v4",
    "$v5",
    "$v6",
    "$v7",
    "$v6,$v7",
    "$v0,$v3"
};

char *sx_fixed_vreg2[] = {
    "",
    "$v6",
    "$v7",
    "$v4",
    "$v5",
    "$v2",
    "$v3",
    "$v0",
    "$v1",
    "$v0,$v1",
    "$v6,$v5"
};

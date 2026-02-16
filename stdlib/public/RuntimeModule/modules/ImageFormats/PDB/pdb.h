//===--- codeview.h - Definitions of CodeView structures for Swift --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions of CodeView structures for import into Swift code
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PDB_H_
#define SWIFT_PDB_H_

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

// -- Multi-stream file definitions --------------------------------------------

#define PDB_PAGE_SIZE     0x1000
#define PDB_MIN_PAGE_SIZE 0x400

// For a v2 MSF
#define PDB_MAX_STREAMS   0x1000
#define PDB_MAX_PAGES     0xffff

// For a v7 MSF
#define PDB_BIG_MAX_STREAMS     0x10000
#define PDB_BIG_MAX_PAGES       0x100000

typedef unsigned short ushort;
typedef uint16_t PN16;
typedef uint16_t SPN16;
typedef uint32_t PN32;
typedef uint32_t SPN32;

typedef PN16  PN;
typedef SPN16 SPN;

typedef PN32  UPN;
typedef SPN32 USPN;

typedef uint16_t SN;
typedef uint32_t UNSN;

#define SN_NIL   0xffff
#define UNSN_NIL 0xffffffff

typedef uint8_t BYTE;
typedef BYTE   *PB;
typedef void   *PV;

typedef uint32_t CB;

typedef struct {
  CB      cb;
  int32_t mpspnpn;
} SI_PERSIST;

typedef struct {
  char       szMagic[0x2c];

  CB         cbPg;              // Size of a page in bytes
  PN         pnFpm;             // Page number of free page map
  PN         pnMac;             // Number of pages in file
  SI_PERSIST siSt;              // Stream table stream

  // Followed by
  // PN         mpspnpn[...];
} MSF_HDR;

typedef struct {
  char       szMagic[0x1e];

  CB         cbPg;      // Size of a page in bytes
  UPN        pnFpm;     // Page number of free page map
  UPN        pnMac;     // Number of pages in file
  SI_PERSIST siSt;      // Stream table stream

  // Followed by
  // PN32       mpspnpnSt[...];
} BIGMSF_HDR;

static const char kPdbHdrMagic[] =
  "Microsoft C/C++ program database 2.00\r\n\x1a\x4a\x47";
static const char kPdbBigHdrMagic[] =
  "Microsoft C/C++ MSF 7.00\r\n\x1a\x44\x53";

static inline bool pdb_is_v2(const char *magic) {
  return memcmp(magic, kPdbHdrMagic, sizeof(kPdbHdrMagic)) == 0;
}

static inline bool pdb_is_v7(const char *magic) {
  return memcmp(magic, kPdbBigHdrMagic, sizeof(kPdbBigHdrMagic)) == 0;
}

// -- PE definitions -----------------------------------------------------------

typedef struct {
  uint8_t Name[8];
  uint32_t VirtualSize;
  uint32_t VirtualAddress;
  uint32_t SizeOfRawData;
  uint32_t PointerToRawData;
  uint32_t PointerToRelocations;
  uint32_t PointerToLinenumbers;
  uint16_t NumberOfRelocations;
  uint16_t NumberOfLinenumbers;
  uint32_t Characteristics;
} PDB_IMAGE_SECTION_HEADER;

// -- CodeView definitions -----------------------------------------------------

enum {
  S_PUB32 = 0x110e,
  S_LPROC32 = 0x110f,
  S_GPROC32 = 0x1110,
  S_LPROC32_ID = 0x1146,
  S_GPROC32_ID = 0x1147,
};

#pragma pack(push, 1)

typedef struct {
  // uint16_t reclen;
  // uint16_t rectyp;
  uint32_t flags;
  uint32_t off;
  uint16_t seg;
} PDB_CV_PUBSYM32;

typedef struct {
  // uint16_t reclen;
  // uint16_t rectyp;
  uint32_t pParent;
  uint32_t pEnd;
  uint32_t pNext;
  uint32_t len;
  uint32_t DbgStart;
  uint32_t DbgEnd;
  uint32_t typind;
  uint32_t off;
  uint16_t seg;
  uint8_t flags;
} PDB_CV_PROCSYM32;

#pragma pack(pop)

enum {
  CV_SIGNATURE_C13 = 4
};

enum {
  DEBUG_S_LINES = 0xf2,         // Holds line number information
  DEBUG_S_FILECHKSMS = 0xf4     // Holds file checksums
};

enum {
  CV_LINES_HAVE_COLUMNS = 0x0001
};

typedef struct {
  uint32_t offCon;
  uint16_t segCon;
  uint16_t flags;
  uint32_t cbCon;
} PDB_CV_LINE_HEADER;

typedef struct {
  uint32_t offFile;     // Offset into file checksum table
  uint32_t nLines;
  uint32_t cbBlock;
  // Followed by
  // PDB_CV_LINE lines[nLines];
  // PDB_CV_COLUMNE columns[nColumns];
} PDB_CV_LINE_BLOCK_HEADER;

typedef struct {
  uint32_t offset;
  uint32_t lineInfo;
} PDB_CV_LINE;

static inline uint32_t pdb_line_start(uint32_t lineInfo) {
  return lineInfo & 0x00ffffff;
}
static inline uint32_t pdb_line_delta(uint32_t lineInfo) {
  return (lineInfo >> 24) & 0x7f;
}
static inline bool pdb_line_is_statement(uint32_t lineInfo) {
  return !!(lineInfo & 0x80000000);
}

typedef struct {
  uint16_t offColumnStart;
  uint16_t offColumnEnd;
} PDB_CV_COLUMN;

enum {
  CHKSUM_TYPE_NONE = 0,
  CHKSUM_TYPE_MD5 = 1,
  CHKSUM_TYPE_SHA1 = 2,
  CHKSUM_TYPE_SHA256 = 3
};

// -- PDB format definitions ---------------------------------------------------

typedef uint32_t IMPV;
typedef uint32_t SIG;
typedef uint32_t AGE;
typedef uint32_t NI;
typedef uint16_t IMOD;
typedef uint16_t ISECT;
typedef uint16_t IFILE;
typedef int32_t  OFF;
typedef uint8_t  SIG70[16];
typedef int32_t  ICH;

typedef struct {
  IMPV impv;    // Implementation version number
  SIG  sig;     // Unique signature
  AGE  age;     // Sequence number (an update count)
} PDB_STREAM;

typedef struct {
  uint32_t ulHdr;
  uint32_t ulVer;
} PDB_NMT_VHDR;

typedef struct {
  SN snGSSyms;                  // Number of global symbols
  SN snPSSyms;                  // Number of public symbols
  SN snSymRecs;                 // Number of symbol records
  CB cbGpModi;                  // Size of the module index substream
  CB cbSC;                      // Size of session contribution substream
  CB cbSecMap;                  // Size of section map substream
  CB cbFileInfo;                // Size of file info substream
} PDB_DBI_HDR;

typedef struct {
  uint32_t verSignature;        // 0xffffffff
  uint32_t verHdr;              // Version number
  AGE      age;                 // Sequence number
  SN       snGSSyms;            // Number of global symbols
  uint16_t usVerAll;            // Version of PDB DLL that built this file?
  SN       snPSSyms;            // Number of public symbols
  uint16_t usVerPdbDllBuild;    // Version of PDB DLL that built this file?
  SN       snSymRecs;           // Number of symbol records
  uint16_t usVerPdbDllRBld;     // Version of PDB DLL that rebuilt this file?
  CB       cbGpModi;            // Size of the module info substream
  CB       cbSC;                // Size of session contribution substream
  CB       cbSecMap;            // Size of segment map substream
  CB       cbFileInfo;          // Size of file info substream
  CB       cbTSMap;             // Size of the type server map substream
  uint32_t iMFC;                // ?
  CB       cbDbgHdr;            // Size of optional DbgHdr substream
  CB       cbECInfo;            // Size of EC(?) substream
  uint16_t flags;               // Miscellaneous flags
  uint16_t wMachine;            // Machine type
  uint32_t rgulReserved[1];     // Reserved for future expansion
} PDB_NEW_DBI_HDR;

typedef struct {
  ISECT isect;
  OFF   off;
  CB    cb;
  IMOD  imod;
} SC20;

typedef struct {
  ISECT    isect;
  OFF      off;
  CB       cb;
  uint32_t dwCharacteristics;
  IMOD     imod;
} SC40;

typedef struct {
  ISECT    isect;
  OFF      off;
  CB       cb;
  uint32_t dwCharacteristics;
  IMOD     imod;
  uint32_t dwDataCrc;
  uint32_t dwRelocCrc;
} SC;

typedef struct {
  ISECT    isect;
  OFF      off;
  CB       cb;
  uint32_t dwCharacteristics;
  IMOD     imod;
  uint32_t dwDataCrc;
  uint32_t dwRelocCrc;
  uint32_t isectCoff;
} SC2;

typedef struct {
  uint32_t pmod;
  SC40     sc;
  uint16_t flags;
  SN       sn;
  CB       cbSyms;
  CB       cbLines;
  CB       cbFpo;
  IFILE    ifileMac;
  uint32_t mpifileichFile;
  // Followed by
  // char szModule[];
  // char szObjFile[];
} MODI50;

typedef struct {
  uint32_t pmod;
  SC       sc;
  uint16_t flags;
  SN       sn;
  CB       cbSyms;
  CB       cbLines;
  CB       cbC13Lines;
  IFILE    ifileMac;
  uint32_t mpifileichFile;
  struct ECInfo {
    NI niSrcFile;
    NI niPdbFile;
  } ecInfo;
  // Followed by
  // char szModule[];
  // char szObjFile[];
} MODI60;

typedef struct {
  uint16_t cSeg;
  uint16_t cSegLog;
} PDB_OMF_SEGMAP;

typedef struct {
  uint16_t flags;
  uint16_t ovl;
  uint16_t group;
  uint16_t frame;
  uint16_t iSegName;
  uint16_t iClassName;
  OFF      offset;
  CB       cbSeg;
} PDB_OMF_SEGMAP_DESC;

#endif /* SWIFT_PDB_H_ */

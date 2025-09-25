//===--- pe-coff.h - Definitions of PE-COFF structures for Swift ----------===//
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
// Definitions of PE-COFF structures for import into Swift code.
//
// We don't just get these from the Windows headers, because we'd like to be
// able to read PE-COFF binaries on non-Windows machines too.
//
// See <https://learn.microsoft.com/en-us/windows/win32/debug/pe-format>
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PE_COFF_H_
#define SWIFT_PE_COFF_H_

#include <inttypes.h>

namespace swift {
namespace runtime {

/* .. Useful macros ......................................................... */

#define PE_ENUM(t,n)   \
  enum __attribute__((enum_extensibility(open))) n: t
#define PE_OPTIONS(t,n) \
  typedef t n; \
  enum __attribute__((flag_enum,enum_extensibility(open))): t

/* .. Constants ............................................................. */

// DOS signature
PE_ENUM(uint16_t, pe_dos_signature) {
  PE_DOS_SIGNATURE = 0x5a4d,  // MZ
};

// PE signature
PE_ENUM(uint32_t, pe_signature) {
  PE_SIGNATURE = 0x00004550, // PE..
};

// Machine types
PE_ENUM(uint16_t, pe_machine) {
  PE_MACHINE_UNKNOWN     = 0,
  PE_MACHINE_ALPHA       = 0x0184,
  PE_MACHINE_ALPHA64     = 0x0284,
  PE_MACHINE_AM33        = 0x01d3,
  PE_MACHINE_AMD64       = 0x8664,
  PE_MACHINE_ARM         = 0x01c0,
  PE_MACHINE_ARM64       = 0xaa64,
  PE_MACHINE_ARM64EC     = 0xa641,
  PE_MACHINE_ARM64X      = 0xa64e,
  PE_MACHINE_ARMNT       = 0x01c4,
  PE_MACHINE_AXP64       = PE_MACHINE_ALPHA64,
  PE_MACHINE_EBC         = 0x0ebc,
  PE_MACHINE_I386        = 0x014c,
  PE_MACHINE_IA64        = 0x0200,
  PE_MACHINE_LOONGARCH32 = 0x6232,
  PE_MACHINE_LOONGARCH64 = 0x6264,
  PE_MACHINE_M32R        = 0x9041,
  PE_MACHINE_MIPS16      = 0x0266,
  PE_MACHINE_MIPSFPU     = 0x0366,
  PE_MACHINE_MIPSFPU16   = 0x0466,
  PE_MACHINE_POWERPC     = 0x01f0,
  PE_MACHINE_POWERPCFP   = 0x01f1,
  PE_MACHINE_R3000BE     = 0x0160,
  PE_MACHINE_R3000       = 0x0162,
  PE_MACHINE_R4000       = 0x0166,
  PE_MACHINE_R10000      = 0x0168,
  PE_MACHINE_RISCV32     = 0x5032,
  PE_MACHINE_RISCV64     = 0x5064,
  PE_MACHINE_RISCV128    = 0x5128,
  PE_MACHINE_SH3         = 0x01a2,
  PE_MACHINE_SH3DSP      = 0x01a3,
  PE_MACHINE_SH4         = 0x01a6,
  PE_MACHINE_SH5         = 0x0108,
  PE_MACHINE_THUMB       = 0x01c2,
  PE_MACHINE_WCEMIPSV2   = 0x0169,
};

// PE header characteristics
PE_OPTIONS(uint16_t, pe_characteristics) {
  PE_RELOCS_STRIPPED         = 0x0001,
  PE_EXECUTABLE_IMAGE        = 0x0002,
  PE_LINE_NUMS_STRIPPED      = 0x0004,
  PE_LOCAL_SYMS_STRIPPED     = 0x0008,
  PE_AGGRESSIVE_WS_TRIM      = 0x0010,
  PE_LARGE_ADDRESS_AWARE     = 0x0020,
  PE_BYTES_REVERSED_LO       = 0x0080,
  PE_32BIT_MACHINE           = 0x0100,
  PE_DEBUG_STRIPPED          = 0x0200,
  PE_REMOVABLE_RUN_FROM_SWAP = 0x0400,
  PE_NET_RUN_FROM_SWAP       = 0x0800,
  PE_SYSTEM                  = 0x1000,
  PE_DLL                     = 0x2000,
  PE_UP_SYSTEM_ONLY          = 0x4000,
  PE_BYTES_REVERSED_HI       = 0x8000,
};

// PE optional header magic
PE_ENUM(uint16_t, pe_optional_magic) {
  PE_PE32 = 0x010b,
  PE_PE32_PLUS = 0x020b,
};

// Windows subsystems
PE_ENUM(uint16_t, pe_subsystem) {
  PE_SUBSYSTEM_UNKNOWN                        = 0,
  PE_SUBSYSTEM_NATIVE                         = 1,
  PE_SUBSYSTEM_WINDOWS_GUI                    = 2,
  PE_SUBSYSTEM_WINDOWS_CUI                    = 3,
  PE_SUBSYSTEM_OS2_CUI                        = 5,
  PE_SUBSYSTEM_POSIX_CUI                      = 7,
  PE_SUBSYSTEM_NATIVE_WINDOWS                 = 8,
  PE_SUBSYSTEM_WINDOWS_CE_GUI                 = 9,
  PE_SUBSYSTEM_EFI_APPLICATION                = 10,
  PE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER        = 11,
  PE_SUBSYSTEM_EFI_RUNTIME_DRIVER             = 12,
  PE_SUBSYSTEM_EFI_ROM                        = 13,
  PE_IMAGE_SUBSYSTEM_XBOX                     = 14,
  PE_IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION = 16,
};

// DLL characteristics
PE_OPTIONS(uint16_t, pe_dll_characteristics) {
  PE_DLL_HIGH_ENTROPY_VA       = 0x0020,
  PE_DLL_DYNAMIC_BASE          = 0x0040,
  PE_DLL_FORCE_INTEGRITY       = 0x0080,
  PE_DLL_NX_COMPAT             = 0x0100,
  PE_DLL_NO_ISOLATION          = 0x0200,
  PE_DLL_NO_SEH                = 0x0400,
  PE_DLL_NO_BIND               = 0x0800,
  PE_DLL_APPCONTAINER          = 0x1000,
  PE_DLL_WDM_DRIVER            = 0x2000,
  PE_DLL_GUARD_CF              = 0x4000,
  PE_DLL_TERMINAL_SERVER_AWARE = 0x8000,
};

// Section characteristics
PE_OPTIONS(uint32_t, pe_section_characteristics) {
  PE_SECTION_TYPE_NO_PAD            = 0x00000008,
  PE_SECTION_CNT_CODE               = 0x00000020,
  PE_SECTION_CNT_INITIALIZED_DATA   = 0x00000040,
  PE_SECTION_CNT_UNINITIALIZED_DATA = 0x00000080,
  PE_SECTION_LNK_OTHER              = 0x00000100,
  PE_SECTION_LNK_INFO               = 0x00000200,
  PE_SECTION_LNK_REMOVE             = 0x00000800,
  PE_SECTION_GPREL                  = 0x00008000,
  PE_SECTION_MEM_PURGEABLE          = 0x00020000,
  PE_SECTION_MEM_LOCKED             = 0x00040000,
  PE_SECTION_MEM_PRELOAD            = 0x00080000,
  PE_SECTION_ALIGNMENT_MASK         = 0x00f00000,
  PE_SECTION_LNK_NRELOC_OVFL        = 0x01000000,
  PE_SECTION_MEM_DISCARDABLE        = 0x02000000,
  PE_SECTION_MEM_NOT_CACHED         = 0x04000000,
  PE_SECTION_MEM_NOT_PAGED          = 0x08000000,
  PE_SECTION_MEM_SHARED             = 0x10000000,
  PE_SECTION_MEM_EXECUTE            = 0x20000000,
  PE_SECTION_MEM_READ               = 0x40000000,
  PE_SECTION_MEM_WRITE              = 0x80000000,
};

static inline uint32_t pe_section_alignment(pe_section_characteristics characteristics) {
  uint32_t alignment = ((characteristics >> 20) & 0xf);
  if (!alignment) return 16;
  return 1 << (alignment - 1);
}

// Symbol types
PE_ENUM(uint16_t, pe_symbol_type) {
  PE_SYMBOL_TYPE_NULL   = 0,
  PE_SYMBOL_TYPE_VOID   = 1,
  PE_SYMBOL_TYPE_CHAR   = 2,
  PE_SYMBOL_TYPE_SHORT  = 3,
  PE_SYMBOL_TYPE_INT    = 4,
  PE_SYMBOL_TYPE_LONG   = 5,
  PE_SYMBOL_TYPE_FLOAT  = 6,
  PE_SYMBOL_TYPE_DOUBLE = 7,
  PE_SYMBOL_TYPE_STRUCT = 8,
  PE_SYMBOL_TYPE_UNION  = 9,
  PE_SYMBOL_TYPE_ENUM   = 10,
  PE_SYMBOL_TYPE_MOE    = 11,
  PE_SYMBOL_TYPE_BYTE   = 12,
  PE_SYMBOL_TYPE_WORD   = 13,
  PE_SYMBOL_TYPE_UINT   = 14,
  PE_SYMBOL_TYPE_DWORD  = 15,
};

// Symbol storage classes
PE_ENUM(uint8_t, pe_symbol_class) {
  PE_SYMBOL_CLASS_END_OF_FUNCTION = 0xff,
  PE_SYMBOL_CLASS_NULL = 0,
  PE_SYMBOL_CLASS_AUTOMATIC = 1,
  PE_SYMBOL_CLASS_EXTERNAL = 2,
  PE_SYMBOL_CLASS_STATIC = 3,
  PE_SYMBOL_CLASS_REGISTER = 4,
  PE_SYMBOL_CLASS_EXTERNAL_DEF = 5,
  PE_SYMBOL_CLASS_LABEL = 6,
  PE_SYMBOL_CLASS_UNDEFINED_LABEL = 7,
  PE_SYMBOL_CLASS_MEMBER_OF_STRUCT = 8,
  PE_SYMBOL_CLASS_ARGUMENT = 9,
  PE_SYMBOL_CLASS_STRUCT_TAG = 10,
  PE_SYMBOL_CLASS_MEMBER_OF_UNION = 11,
  PE_SYMBOL_CLASS_UNION_TAG = 12,
  PE_SYMBOL_CLASS_TYPE_DEFINITION = 13,
  PE_SYMBOL_CLASS_UNDEFINED_STATIC = 14,
  PE_SYMBOL_CLASS_ENUM_TAG = 15,
  PE_SYMBOL_CLASS_MEMBER_OF_ENUM = 16,
  PE_SYMBOL_CLASS_REGISTER_PARAM = 17,
  PE_SYMBOL_CLASS_BIT_FIELD = 18,
  PE_SYMBOL_CLASS_BLOCK = 100,
  PE_SYMBOL_CLASS_FUNCTION = 101,
  PE_SYMBOL_CLASS_END_OF_STRUCT = 102,
  PE_SYMBOL_CLASS_FILE = 103,
  PE_SYMBOL_CLASS_SECTION = 104,
  PE_SYMBOL_CLASS_WEAK_EXTERNAL = 105,
  PE_SYMBOL_CLASS_CLR_TOKEN = 107,
};

PE_ENUM(uint32_t, pe_debug_directory_type) {
  PE_DEBUG_TYPE_UNKNOWN               = 0,
  PE_DEBUG_TYPE_COFF                  = 1,
  PE_DEBUG_TYPE_CODEVIEW              = 2,
  PE_DEBUG_TYPE_FPO                   = 3,
  PE_DEBUG_TYPE_MISC                  = 4,
  PE_DEBUG_TYPE_EXCEPTION             = 5,
  PE_DEBUG_TYPE_FIXUP                 = 6,
  PE_DEBUG_TYPE_OMAP_TO_SRC           = 7,
  PE_DEBUG_TYPE_OMAP_FROM_SRC         = 8,
  PE_DEBUG_TYPE_BORLAND               = 9,
  PE_DEBUG_TYPE_BBT                   = 10,
  PE_DEBUG_TYPE_CLSID                 = 11,
  PE_DEBUG_TYPE_VC_FEATURE            = 12,
  PE_DEBUG_TYPE_POGO                  = 13,
  PE_DEBUG_TYPE_ILTCG                 = 14,
  PE_DEBUG_TYPE_MPX                   = 15,
  PE_DEBUG_TYPE_REPRO                 = 16,
  PE_DEBUG_TYPE_SPGO                  = 18,
  PE_DEBUG_TYPE_EX_DLLCHARACTERISTICS = 20
};

PE_OPTIONS(uint32_t, pe_debug_characteristics) {
};

/* .. PE Header ............................................................. */

// DOS header - the only thing we actually care about is the offset to the PE
// header at 0x3c.
struct pe_dos_header {
  pe_dos_signature   e_magic;     // Magic number
  uint16_t           e_cblp;      // Count of bytes in last page
  uint16_t           e_cp;        // Count of pages
  uint16_t           e_crlc;      // Count of relocations
  uint16_t           e_cparhdr;   // Count of paragraphs in header
  uint16_t           e_minalloc;  // Minimum allocation in paragraphs
  uint16_t           e_maxalloc;  // Maximum allocation in paragraphs
  uint16_t           e_ss;        // Initial stack segment
  uint16_t           e_sp;        // Initial stack pointer
  uint16_t           e_csum;      // Checksum
  uint16_t           e_ip;        // Initial instruction pointer
  uint16_t           e_cs;        // Initial code segment
  uint16_t           e_lfarlc;    // Offset to relocation table
  uint16_t           e_ovno;      // Overlay number
  uint16_t           e_res[4];    // Reserved
  uint16_t           e_oemid;     // OEM identifier
  uint16_t           e_oeminfo;   // OEM information
  uint16_t           e_res2[10];  // Reserved
  uint32_t           e_lfanew;    // Offset to PE header
};

struct pe_header {
  pe_machine           Machine;
  uint16_t             NumberOfSections;
  uint32_t             TimeDateStamp;
  uint32_t             PointerToSymbolTable;
  uint32_t             NumberOfSymbols;
  uint16_t             SizeOfOptionalHeader;
  pe_characteristics   Characteristics;
};

struct pe_optional_header {
  pe_optional_magic    Magic;
  uint8_t              MajorLinkerVersion;
  uint8_t              MinorLinkerVersion;
  uint32_t             SizeOfCode;
  uint32_t             SizeOfInitializedData;
  uint32_t             SizeOfUninitializedData;
  uint32_t             AddressOfEntryPoint;
  uint32_t             BaseOfCode;
};

struct pe_data_directory_entry {
  uint32_t VirtualAddress;
  uint32_t Size;
};

struct pe_windows_header32 {
  uint32_t                 ImageBase;
  uint32_t                 SectionAlignment;
  uint32_t                 FileAlignment;
  uint16_t                 MajorOperatingSystemVersion;
  uint16_t                 MinorOperatingSystemVersion;
  uint16_t                 MajorImageVersion;
  uint16_t                 MinorImageVersion;
  uint16_t                 MajorSubsystemVersion;
  uint16_t                 MinorSubsystemVersion;
  uint32_t                 Win32VersionValue;
  uint32_t                 SizeOfImage;
  uint32_t                 SizeOfHeaders;
  uint32_t                 CheckSum;
  pe_subsystem             Subsystem;
  pe_dll_characteristics   DllCharacteristics;
  uint32_t                 SizeOfStackReserve;
  uint32_t                 SizeOfStackCommit;
  uint32_t                 SizeOfHeapReserve;
  uint32_t                 SizeOfHeapCommit;
  uint32_t                 LoaderFlags;
  uint32_t                 NumberOfRvaAndSizes;

  // Followed by:
  // pe_data_directory_entry DataDirectory[NumberOfRvaAndSizes];
} pe_windows_header32;

struct pe_windows_header64 {
  uint64_t                 ImageBase;
  uint32_t                 SectionAlignment;
  uint32_t                 FileAlignment;
  uint16_t                 MajorOperatingSystemVersion;
  uint16_t                 MinorOperatingSystemVersion;
  uint16_t                 MajorImageVersion;
  uint16_t                 MinorImageVersion;
  uint16_t                 MajorSubsystemVersion;
  uint16_t                 MinorSubsystemVersion;
  uint32_t                 Win32VersionValue;
  uint32_t                 SizeOfImage;
  uint32_t                 SizeOfHeaders;
  uint32_t                 CheckSum;
  pe_subsystem             Subsystem;
  pe_dll_characteristics   DllCharacteristics;
  uint64_t                 SizeOfStackReserve;
  uint64_t                 SizeOfStackCommit;
  uint64_t                 SizeOfHeapReserve;
  uint64_t                 SizeOfHeapCommit;
  uint32_t                 LoaderFlags;
  uint32_t                 NumberOfRvaAndSizes;

  // Followed by:
  // pe_data_directory_entry DataDirectory[NumberOfRvaAndSizes];
};

struct pe_section {
  char                         Name[8];
  uint32_t                     VirtualSize;
  uint32_t                     VirtualAddress;
  uint32_t                     SizeOfRawData;
  uint32_t                     PointerToRawData;
  uint32_t                     PointerToRelocations;
  uint32_t                     PointerToLinenumbers;
  uint32_t                     NumberOfRelocations;
  uint32_t                     NumberOfLinenumbers;
  pe_section_characteristics   Characteristics;
};

/* .. Debug Directory ....................................................... */

struct pe_debug_directory {
  pe_debug_characteristics    Characteristics;
  uint32_t                    TimeDateStamp;
  uint16_t                    MajorVersion;
  uint16_t                    MinorVersion;
  pe_debug_directory_type     Type;
  uint32_t                    SizeOfData;
  uint32_t                    AddressOfRawData;
  uint32_t                    PointerToRawData;
};

/* .. COFF symbol table ..................................................... */

struct pe_symbol {
  // This is either up to eight characters of short name, or four zero bytes
  // followed by an offset into the string table
  union {
    char ShortName[8];
    struct {
      uint32_t Zeroes;
      uint32_t Offset;  // Offset into the string table
    } LongName;
  };
  uint32_t          Value;
  int16_t           SectionNumber;
  pe_symbol_type    Type;
  pe_symbol_class   StorageClass;
  uint8_t           NumberOfAuxSymbols;
};

} // namespace runtime
} // namespace swift

#endif // SWIFT_PE_COFF_H_


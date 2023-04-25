//===--- dwarf.h - Definitions of DWARF structures for import into Swift --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions of DWARF structures for import into Swift code
//
// The types here are taken from the DWARF 5 specification, from
// <https://dwarfstd.org>
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DWARF_H
#define SWIFT_DWARF_H

#include <inttypes.h>

#pragma pack(push, 1)

/* .. Useful macros ......................................................... */

#define DWARF_ENUM(t,n) \
  enum __attribute__((enum_extensibility(open))) n: t
#define DWARF_BYTECODE \
  __attribute__((enum_extensibility(open))) : Dwarf_Byte

/* .. Data Representation ................................................... */

// Common sizes (these don't change between 32-bit and 64-bit)
typedef uint8_t  Dwarf_Byte;
typedef uint16_t Dwarf_Half;
typedef uint32_t Dwarf_Word;
typedef uint64_t Dwarf_Xword;
typedef int8_t   Dwarf_Sbyte;
typedef int32_t  Dwarf_Sword;
typedef int64_t  Dwarf_Sxword;

// 32-bit sizes
typedef uint32_t Dwarf32_Offset;
typedef uint32_t Dwarf32_Size;
typedef uint32_t Dwarf32_Length; // 0xfffffff0 and above are reserved

// 64-bit sizes
typedef uint64_t Dwarf64_Offset;
typedef uint64_t Dwarf64_Size;
typedef struct {
  uint32_t magic;  // 0xffffffff means we're using 64-bit
  uint64_t length;
} Dwarf64_Length;

/* .. Form Encodings ........................................................ */

// Table 7.5.6
enum DWARF_BYTECODE {
  DW_FORM_addr           = 0x01,
  // Reserved            = 0x02,
  DW_FORM_block2         = 0x03,
  DW_FORM_block4         = 0x04,
  DW_FORM_data2          = 0x05,
  DW_FORM_data4          = 0x06,
  DW_FORM_data8          = 0x07,
  DW_FORM_string         = 0x08,
  DW_FORM_block          = 0x09,
  DW_FORM_block1         = 0x0a,
  DW_FORM_data1          = 0x0b,
  DW_FORM_flag           = 0x0c,
  DW_FORM_sdata          = 0x0d,
  DW_FORM_strp           = 0x0e,
  DW_FORM_udata          = 0x0f,
  DW_FORM_ref_addr       = 0x10,
  DW_FORM_ref1           = 0x11,
  DW_FORM_ref2           = 0x12,
  DW_FORM_ref4           = 0x13,
  DW_FORM_ref8           = 0x14,
  DW_FORM_ref_udata      = 0x15,
  DW_FORM_indirect       = 0x16,
  DW_FORM_sec_offset     = 0x17,
  DW_FORM_exprloc        = 0x18,
  DW_FORM_flag_present   = 0x19,
  DW_FORM_strx           = 0x1a,
  DW_FORM_addrx          = 0x1b,
  DW_FORM_ref_sup4       = 0x1c,
  DW_FORM_strp_sup       = 0x1d,
  DW_FORM_data16         = 0x1e,
  DW_FORM_line_strp      = 0x1f,
  DW_FORM_ref_sig8       = 0x20,
  DW_FORM_implicit_const = 0x21,
  DW_FORM_loclistx       = 0x22,
  DW_FORM_rnglistx       = 0x23,
  DW_FORM_ref_sup8       = 0x24,
  DW_FORM_strx1          = 0x25,
  DW_FORM_strx2          = 0x26,
  DW_FORM_strx3          = 0x27,
  DW_FORM_strx4          = 0x28,
  DW_FORM_addrx1         = 0x29,
  DW_FORM_addrx2         = 0x2a,
  DW_FORM_addrx3         = 0x2b,
  DW_FORM_addrx4         = 0x2c,
};

/* .. DWARF Expressions ..................................................... */

// Table 7.9
enum DWARF_BYTECODE {
  // Reserved               = 0x01,
  // Reserved               = 0x02,
  DW_OP_addr                = 0x03,
  // Reserved               = 0x04,
  // Reserved               = 0x05,
  DW_OP_deref               = 0x06,
  // Reserved               = 0x07,
  DW_OP_const1u             = 0x08,
  DW_OP_const1s             = 0x09,
  DW_OP_const2u             = 0x0a,
  DW_OP_const2s             = 0x0b,
  DW_OP_const4u             = 0x0c,
  DW_OP_const4s             = 0x0d,
  DW_OP_const8u             = 0x0e,
  DW_OP_const8s             = 0x0f,
  DW_OP_constu              = 0x10,
  DW_OP_consts              = 0x11,
  DW_OP_dup                 = 0x12,
  DW_OP_drop                = 0x13,
  DW_OP_over                = 0x14,
  DW_OP_pick                = 0x15,
  DW_OP_swap                = 0x16,
  DW_OP_rot                 = 0x17,
  DW_OP_xderef              = 0x18,
  DW_OP_abs                 = 0x19,
  DW_OP_and                 = 0x1a,
  DW_OP_div                 = 0x1b,
  DW_OP_minus               = 0x1c,
  DW_OP_mod                 = 0x1d,
  DW_OP_mul                 = 0x1e,
  DW_OP_neg                 = 0x1f,
  DW_OP_not                 = 0x20,
  DW_OP_or                  = 0x21,
  DW_OP_plus                = 0x22,
  DW_OP_plus_uconst         = 0x23,
  DW_OP_shl                 = 0x24,
  DW_OP_shr                 = 0x25,
  DW_OP_shra                = 0x26,
  DW_OP_xor                 = 0x27,
  DW_OP_bra                 = 0x28,
  DW_OP_eq                  = 0x29,
  DW_OP_ge                  = 0x2a,
  DW_OP_gt                  = 0x2b,
  DW_OP_le                  = 0x2c,
  DW_OP_lt                  = 0x2d,
  DW_OP_ne                  = 0x2e,
  DW_OP_skip                = 0x2f,

  DW_OP_lit0                = 0x30,
  DW_OP_lit1                = 0x31,
  DW_OP_lit2                = 0x32,
  DW_OP_lit3                = 0x33,
  DW_OP_lit4                = 0x34,
  DW_OP_lit5                = 0x35,
  DW_OP_lit6                = 0x36,
  DW_OP_lit7                = 0x37,
  DW_OP_lit8                = 0x38,
  DW_OP_lit9                = 0x39,
  DW_OP_lit10               = 0x3a,
  DW_OP_lit11               = 0x3b,
  DW_OP_lit12               = 0x3c,
  DW_OP_lit13               = 0x3d,
  DW_OP_lit14               = 0x3e,
  DW_OP_lit15               = 0x3f,
  DW_OP_lit16               = 0x40,
  DW_OP_lit17               = 0x41,
  DW_OP_lit18               = 0x42,
  DW_OP_lit19               = 0x43,
  DW_OP_lit20               = 0x44,
  DW_OP_lit21               = 0x45,
  DW_OP_lit22               = 0x46,
  DW_OP_lit23               = 0x47,
  DW_OP_lit24               = 0x48,
  DW_OP_lit25               = 0x49,
  DW_OP_lit26               = 0x4a,
  DW_OP_lit27               = 0x4b,
  DW_OP_lit28               = 0x4c,
  DW_OP_lit29               = 0x4d,
  DW_OP_lit30               = 0x4e,
  DW_OP_lit31               = 0x4f,

  DW_OP_reg0                = 0x50,
  DW_OP_reg1                = 0x51,
  DW_OP_reg2                = 0x52,
  DW_OP_reg3                = 0x53,
  DW_OP_reg4                = 0x54,
  DW_OP_reg5                = 0x55,
  DW_OP_reg6                = 0x56,
  DW_OP_reg7                = 0x57,
  DW_OP_reg8                = 0x58,
  DW_OP_reg9                = 0x59,
  DW_OP_reg10               = 0x5a,
  DW_OP_reg11               = 0x5b,
  DW_OP_reg12               = 0x5c,
  DW_OP_reg13               = 0x5d,
  DW_OP_reg14               = 0x5e,
  DW_OP_reg15               = 0x5f,
  DW_OP_reg16               = 0x60,
  DW_OP_reg17               = 0x61,
  DW_OP_reg18               = 0x62,
  DW_OP_reg19               = 0x63,
  DW_OP_reg20               = 0x64,
  DW_OP_reg21               = 0x65,
  DW_OP_reg22               = 0x66,
  DW_OP_reg23               = 0x67,
  DW_OP_reg24               = 0x68,
  DW_OP_reg25               = 0x69,
  DW_OP_reg26               = 0x6a,
  DW_OP_reg27               = 0x6b,
  DW_OP_reg28               = 0x6c,
  DW_OP_reg29               = 0x6d,
  DW_OP_reg30               = 0x6e,
  DW_OP_reg31               = 0x6f,

  DW_OP_breg0               = 0x70,
  DW_OP_breg1               = 0x71,
  DW_OP_breg2               = 0x72,
  DW_OP_breg3               = 0x73,
  DW_OP_breg4               = 0x74,
  DW_OP_breg5               = 0x75,
  DW_OP_breg6               = 0x76,
  DW_OP_breg7               = 0x77,
  DW_OP_breg8               = 0x78,
  DW_OP_breg9               = 0x79,
  DW_OP_breg10              = 0x7a,
  DW_OP_breg11              = 0x7b,
  DW_OP_breg12              = 0x7c,
  DW_OP_breg13              = 0x7d,
  DW_OP_breg14              = 0x7e,
  DW_OP_breg15              = 0x7f,
  DW_OP_breg16              = 0x80,
  DW_OP_breg17              = 0x81,
  DW_OP_breg18              = 0x82,
  DW_OP_breg19              = 0x83,
  DW_OP_breg20              = 0x84,
  DW_OP_breg21              = 0x85,
  DW_OP_breg22              = 0x86,
  DW_OP_breg23              = 0x87,
  DW_OP_breg24              = 0x88,
  DW_OP_breg25              = 0x89,
  DW_OP_breg26              = 0x8a,
  DW_OP_breg27              = 0x8b,
  DW_OP_breg28              = 0x8c,
  DW_OP_breg29              = 0x8d,
  DW_OP_breg30              = 0x8e,
  DW_OP_breg31              = 0x8f,

  DW_OP_regx                = 0x90,
  DW_OP_fbreg               = 0x91,
  DW_OP_bregx               = 0x92,

  DW_OP_piece               = 0x93,
  DW_OP_deref_size          = 0x94,
  DW_OP_xderef_size         = 0x95,
  DW_OP_nop                 = 0x96,
  DW_OP_push_object_address = 0x97,
  DW_OP_call2               = 0x98,
  DW_OP_call4               = 0x99,
  DW_OP_call_ref            = 0x9a,
  DW_OP_form_tls_address    = 0x9b,
  DW_OP_call_frame_cfa      = 0x9c,
  DW_OP_bit_piece           = 0x9d,
  DW_OP_implicit_value      = 0x9e,
  DW_OP_stack_value         = 0x9f,
  DW_OP_implicit_pointer    = 0xa0,
  DW_OP_addrx               = 0xa1,
  DW_OP_constx              = 0xa2,
  DW_OP_entry_value         = 0xa3,
  DW_OP_const_type          = 0xa4,
  DW_OP_regval_type         = 0xa5,
  DW_OP_deref_type          = 0xa6,
  DW_OP_xderef_type         = 0xa7,
  DW_OP_convert             = 0xa8,
  DW_OP_reinterpret         = 0xa9,
  DW_OP_lo_user             = 0xe0,
  DW_OP_hi_user             = 0xff,
};

/* .. Line Number Information ............................................... */

// Table 7.25
enum DWARF_BYTECODE {
  DW_LNS_extended           = 0x00,
  DW_LNS_copy               = 0x01,
  DW_LNS_advance_pc         = 0x02,
  DW_LNS_advance_line       = 0x03,
  DW_LNS_set_file           = 0x04,
  DW_LNS_set_column         = 0x05,
  DW_LNS_negate_stmt        = 0x06,
  DW_LNS_set_basic_block    = 0x07,
  DW_LNS_const_add_pc       = 0x08,
  DW_LNS_fixed_advance_pc   = 0x09,
  DW_LNS_set_prologue_end   = 0x0a,
  DW_LNS_set_epilogue_begin = 0x0b,
  DW_LNS_set_isa            = 0x0c,
};

// Table 7.26
enum DWARF_BYTECODE {
  DW_LNE_end_sequence      = 0x01,
  DW_LNE_set_address       = 0x02,
  // Reserved              = 0x03,
  DW_LNE_set_discriminator = 0x04,
  DW_LNE_lo_user           = 0x80,
  DW_LNE_hi_user           = 0xff,
};

// Table 7.27
typedef DWARF_ENUM(Dwarf_Half, Dwarf_Lhdr_Format) {
  DW_LNCT_path            = 0x0001,
  DW_LNCT_directory_index = 0x0002,
  DW_LNCT_timestamp       = 0x0003,
  DW_LNCT_size            = 0x0004,
  DW_LNCT_MD5             = 0x0005,
  DW_LNCT_lo_user         = 0x2000,
  DW_LNCT_hi_user         = 0x3fff,
} Dwarf_Lhdr_Format;

// 6.2.4 The Line Number Program Header
typedef struct {
  Dwarf32_Length length;
  Dwarf_Half version;
  Dwarf_Byte address_size;
  Dwarf_Byte segment_selector_size;
  Dwarf32_Size header_length;
  Dwarf_Byte minimum_instruction_length;
  Dwarf_Byte maximum_operations_per_instruction;
  Dwarf_Byte default_is_stmt;
  Dwarf_Sbyte line_base;
  Dwarf_Byte line_range;
  Dwarf_Byte opcode_base;

  // Variable length fields:
  // Dwarf_Byte standard_opcode_lengths[];
  // Dwarf_Byte directory_entry_format_count;
  // Dwarf_ULEB128 directory_entry_format[];
  // Dwarf_ULEB128 directories_count;
  // encoded directories[];
  // Dwarf_Byte file_name_entry_format_count;
  // Dwarf_ULEB128 file_name_entry_format;
  // Dwarf_ULEB128 file_names_count;
  // encoded file_names[];
} DWARF32_Lhdr;

typedef struct {
  Dwarf64_Length length;
  Dwarf_Half version;
  Dwarf_Byte address_size;
  Dwarf_Byte segment_selector_size;
  Dwarf64_Size header_length;
  Dwarf_Byte minimum_instruction_length;
  Dwarf_Byte maximum_operations_per_instruction;
  Dwarf_Byte default_is_stmt;
  Dwarf_Sbyte line_base;
  Dwarf_Byte line_range;
  Dwarf_Byte opcode_base;

  // Variable length fields:
  // Dwarf_Byte standard_opcode_lengths[];
  // Dwarf_Byte directory_entry_format_count;
  // Dwarf_ULEB128 directory_entry_format[];
  // Dwarf_ULEB128 directories_count;
  // encoded directories[];
  // Dwarf_Byte file_name_entry_format_count;
  // Dwarf_ULEB128 file_name_entry_format;
  // Dwarf_ULEB128 file_names_count;
  // encoded file_names[];
} DWARF64_Lhdr;

/* .. Call frame instruction encodings ...................................... */

// Table 7.29
enum DWARF_BYTECODE {
  DW_CFA_advance_loc        = 0x40,
  DW_CFA_offset             = 0x80,
  DW_CFA_restore            = 0xc0,
  DW_CFA_nop                = 0x00,
  DW_CFA_set_loc            = 0x01,
  DW_CFA_advance_loc1       = 0x02,
  DW_CFA_advance_loc2       = 0x03,
  DW_CFA_advance_loc4       = 0x04,
  DW_CFA_offset_extended    = 0x05,
  DW_CFA_restore_extended   = 0x06,
  DW_CFA_undefined          = 0x07,
  DW_CFA_same_value         = 0x08,
  DW_CFA_register           = 0x09,
  DW_CFA_remember_state     = 0x0a,
  DW_CFA_restore_state      = 0x0b,
  DW_CFA_def_cfa            = 0x0c,
  DW_CFA_def_cfa_register   = 0x0d,
  DW_CFA_def_cfa_offset     = 0x0e,
  DW_CFA_def_cfa_expression = 0x0f,
  DW_CFA_expression         = 0x10,
  DW_CFA_offset_extended_sf = 0x11,
  DW_CFA_def_cfa_sf         = 0x12,
  DW_CFA_def_cfa_offset_sf  = 0x13,
  DW_CFA_val_offset         = 0x14,
  DW_CFA_val_offset_sf      = 0x15,
  DW_CFA_val_expression     = 0x16,
  DW_CFA_lo_user            = 0x1c,
  DW_CFA_hi_user            = 0x3f
};

/* .. Common Information Entry .............................................. */

// Table 4.5: Common Information Entry (CIE)
typedef struct {
  Dwarf32_Length length;
  Dwarf32_Offset CIE_id;
  Dwarf_Byte     version;

  // Followed by variable length fields as follows:
  //
  // Dwarf_Byte augmentation[]; // NUL terminated string
  // Dwarf_Byte address_size;
  // Dwarf_Byte segment_selector_size;
  // Dwarf_ULEB128 code_alignment_factor;
  // Dwarf_SLEB128 data_alignment_factor;
  // Dwarf_ULEB128 return_address_register;
  // Dwarf_Byte initial_instructions[];
  // Dwarf_Byte padding[]
} Dwarf32_CIEHdr;

typedef struct {
  Dwarf64_Length length;
  Dwarf64_Offset CIE_id;
  Dwarf_Byte     version;

  // Followed by variable length fields as follows:
  //
  // Dwarf_Byte augmentation[]; // NUL terminated string
  // Dwarf_Byte address_size;
  // Dwarf_Byte segment_selector_size;
  // Dwarf_ULEB128 code_alignment_factor;
  // Dwarf_SLEB128 data_alignment_factor;
  // Dwarf_ULEB128 return_address_register;
  // Dwarf_Byte initial_instructions[];
  // Dwarf_Byte padding[]
} Dwarf64_CIEHdr;

/* .. Frame Descriptor Entry ................................................ */

// Table 4.7: Frame Descriptor Entry (FDE)
typedef struct {
  Dwarf32_Length length;
  Dwarf32_Offset CIE_pointer;   // Offset into the section that points at CIE

  // Followed by variable fields as follows:
  //
  // Dwarf_FarAddr initial_location; // May include segment selector and address
  // Dwarf_Addr    address_range;    // Number of bytes of instructions
  // Dwarf_Byte    instructions[];
  // Dwarf_Byte    padding[];
} Dwarf32_FDEHdr;

#pragma pack(pop)

#endif // SWIFT_DWARF_H

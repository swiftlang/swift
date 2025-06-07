//===--- Dwarf.swift - DWARF support for Swift ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines various DWARF structures and provides types for working with
// DWARF data on disk and in memory.
//
//===----------------------------------------------------------------------===//

import Swift

internal import BacktracingImpl.ImageFormats.Dwarf

// .. Use *our* Dwarf definitions ..............................................

// To avoid confusion with other similar sets of definitions, we've put ours
// into a C++ wrapper, which means we need aliases here.

typealias Dwarf_Byte   = swift.runtime.Dwarf_Byte
typealias Dwarf_Half   = swift.runtime.Dwarf_Half
typealias Dwarf_Word   = swift.runtime.Dwarf_Word
typealias Dwarf_Xword  = swift.runtime.Dwarf_Xword
typealias Dwarf_Sbyte  = swift.runtime.Dwarf_Sbyte
typealias Dwarf_Sword  = swift.runtime.Dwarf_Sword
typealias Dwarf_Sxword = swift.runtime.Dwarf_Sxword

typealias Dwarf32_Offset = swift.runtime.Dwarf32_Offset
typealias Dwarf32_Size   = swift.runtime.Dwarf32_Size
typealias Dwarf32_Length = swift.runtime.Dwarf32_Length

typealias Dwarf64_Offset = swift.runtime.Dwarf64_Offset
typealias Dwarf64_Size   = swift.runtime.Dwarf64_Size
typealias Dwarf64_Length = swift.runtime.Dwarf64_Length

typealias Dwarf_UnitType           = swift.runtime.Dwarf_UnitType
typealias Dwarf_Tag                = swift.runtime.Dwarf_Tag
typealias Dwarf_ChildDetermination = swift.runtime.Dwarf_ChildDetermination
typealias Dwarf_Attribute          = swift.runtime.Dwarf_Attribute
typealias Dwarf_Form               = swift.runtime.Dwarf_Form

let DW_OP_addr                = swift.runtime.DW_OP_addr
let DW_OP_deref               = swift.runtime.DW_OP_deref
let DW_OP_const1u             = swift.runtime.DW_OP_const1u
let DW_OP_const1s             = swift.runtime.DW_OP_const1s
let DW_OP_const2u             = swift.runtime.DW_OP_const2u
let DW_OP_const2s             = swift.runtime.DW_OP_const2s
let DW_OP_const4u             = swift.runtime.DW_OP_const4u
let DW_OP_const4s             = swift.runtime.DW_OP_const4s
let DW_OP_const8u             = swift.runtime.DW_OP_const8u
let DW_OP_const8s             = swift.runtime.DW_OP_const8s
let DW_OP_constu              = swift.runtime.DW_OP_constu
let DW_OP_consts              = swift.runtime.DW_OP_consts
let DW_OP_dup                 = swift.runtime.DW_OP_dup
let DW_OP_drop                = swift.runtime.DW_OP_drop
let DW_OP_over                = swift.runtime.DW_OP_over
let DW_OP_pick                = swift.runtime.DW_OP_pick
let DW_OP_swap                = swift.runtime.DW_OP_swap
let DW_OP_rot                 = swift.runtime.DW_OP_rot
let DW_OP_xderef              = swift.runtime.DW_OP_xderef
let DW_OP_abs                 = swift.runtime.DW_OP_abs
let DW_OP_and                 = swift.runtime.DW_OP_and
let DW_OP_div                 = swift.runtime.DW_OP_div
let DW_OP_minus               = swift.runtime.DW_OP_minus
let DW_OP_mod                 = swift.runtime.DW_OP_mod
let DW_OP_mul                 = swift.runtime.DW_OP_mul
let DW_OP_neg                 = swift.runtime.DW_OP_neg
let DW_OP_not                 = swift.runtime.DW_OP_not
let DW_OP_or                  = swift.runtime.DW_OP_or
let DW_OP_plus                = swift.runtime.DW_OP_plus
let DW_OP_plus_uconst         = swift.runtime.DW_OP_plus_uconst
let DW_OP_shl                 = swift.runtime.DW_OP_shl
let DW_OP_shr                 = swift.runtime.DW_OP_shr
let DW_OP_shra                = swift.runtime.DW_OP_shra
let DW_OP_xor                 = swift.runtime.DW_OP_xor
let DW_OP_bra                 = swift.runtime.DW_OP_bra
let DW_OP_eq                  = swift.runtime.DW_OP_eq
let DW_OP_ge                  = swift.runtime.DW_OP_ge
let DW_OP_gt                  = swift.runtime.DW_OP_gt
let DW_OP_le                  = swift.runtime.DW_OP_le
let DW_OP_lt                  = swift.runtime.DW_OP_lt
let DW_OP_ne                  = swift.runtime.DW_OP_ne
let DW_OP_skip                = swift.runtime.DW_OP_skip
let DW_OP_lit0                = swift.runtime.DW_OP_lit0
let DW_OP_lit1                = swift.runtime.DW_OP_lit1
let DW_OP_lit2                = swift.runtime.DW_OP_lit2
let DW_OP_lit3                = swift.runtime.DW_OP_lit3
let DW_OP_lit4                = swift.runtime.DW_OP_lit4
let DW_OP_lit5                = swift.runtime.DW_OP_lit5
let DW_OP_lit6                = swift.runtime.DW_OP_lit6
let DW_OP_lit7                = swift.runtime.DW_OP_lit7
let DW_OP_lit8                = swift.runtime.DW_OP_lit8
let DW_OP_lit9                = swift.runtime.DW_OP_lit9
let DW_OP_lit10               = swift.runtime.DW_OP_lit10
let DW_OP_lit11               = swift.runtime.DW_OP_lit11
let DW_OP_lit12               = swift.runtime.DW_OP_lit12
let DW_OP_lit13               = swift.runtime.DW_OP_lit13
let DW_OP_lit14               = swift.runtime.DW_OP_lit14
let DW_OP_lit15               = swift.runtime.DW_OP_lit15
let DW_OP_lit16               = swift.runtime.DW_OP_lit16
let DW_OP_lit17               = swift.runtime.DW_OP_lit17
let DW_OP_lit18               = swift.runtime.DW_OP_lit18
let DW_OP_lit19               = swift.runtime.DW_OP_lit19
let DW_OP_lit20               = swift.runtime.DW_OP_lit20
let DW_OP_lit21               = swift.runtime.DW_OP_lit21
let DW_OP_lit22               = swift.runtime.DW_OP_lit22
let DW_OP_lit23               = swift.runtime.DW_OP_lit23
let DW_OP_lit24               = swift.runtime.DW_OP_lit24
let DW_OP_lit25               = swift.runtime.DW_OP_lit25
let DW_OP_lit26               = swift.runtime.DW_OP_lit26
let DW_OP_lit27               = swift.runtime.DW_OP_lit27
let DW_OP_lit28               = swift.runtime.DW_OP_lit28
let DW_OP_lit29               = swift.runtime.DW_OP_lit29
let DW_OP_lit30               = swift.runtime.DW_OP_lit30
let DW_OP_lit31               = swift.runtime.DW_OP_lit31

let DW_OP_reg0                = swift.runtime.DW_OP_reg0
let DW_OP_reg1                = swift.runtime.DW_OP_reg1
let DW_OP_reg2                = swift.runtime.DW_OP_reg2
let DW_OP_reg3                = swift.runtime.DW_OP_reg3
let DW_OP_reg4                = swift.runtime.DW_OP_reg4
let DW_OP_reg5                = swift.runtime.DW_OP_reg5
let DW_OP_reg6                = swift.runtime.DW_OP_reg6
let DW_OP_reg7                = swift.runtime.DW_OP_reg7
let DW_OP_reg8                = swift.runtime.DW_OP_reg8
let DW_OP_reg9                = swift.runtime.DW_OP_reg9
let DW_OP_reg10               = swift.runtime.DW_OP_reg10
let DW_OP_reg11               = swift.runtime.DW_OP_reg11
let DW_OP_reg12               = swift.runtime.DW_OP_reg12
let DW_OP_reg13               = swift.runtime.DW_OP_reg13
let DW_OP_reg14               = swift.runtime.DW_OP_reg14
let DW_OP_reg15               = swift.runtime.DW_OP_reg15
let DW_OP_reg16               = swift.runtime.DW_OP_reg16
let DW_OP_reg17               = swift.runtime.DW_OP_reg17
let DW_OP_reg18               = swift.runtime.DW_OP_reg18
let DW_OP_reg19               = swift.runtime.DW_OP_reg19
let DW_OP_reg20               = swift.runtime.DW_OP_reg20
let DW_OP_reg21               = swift.runtime.DW_OP_reg21
let DW_OP_reg22               = swift.runtime.DW_OP_reg22
let DW_OP_reg23               = swift.runtime.DW_OP_reg23
let DW_OP_reg24               = swift.runtime.DW_OP_reg24
let DW_OP_reg25               = swift.runtime.DW_OP_reg25
let DW_OP_reg26               = swift.runtime.DW_OP_reg26
let DW_OP_reg27               = swift.runtime.DW_OP_reg27
let DW_OP_reg28               = swift.runtime.DW_OP_reg28
let DW_OP_reg29               = swift.runtime.DW_OP_reg29
let DW_OP_reg30               = swift.runtime.DW_OP_reg30
let DW_OP_reg31               = swift.runtime.DW_OP_reg31

let DW_OP_breg0               = swift.runtime.DW_OP_breg0
let DW_OP_breg1               = swift.runtime.DW_OP_breg1
let DW_OP_breg2               = swift.runtime.DW_OP_breg2
let DW_OP_breg3               = swift.runtime.DW_OP_breg3
let DW_OP_breg4               = swift.runtime.DW_OP_breg4
let DW_OP_breg5               = swift.runtime.DW_OP_breg5
let DW_OP_breg6               = swift.runtime.DW_OP_breg6
let DW_OP_breg7               = swift.runtime.DW_OP_breg7
let DW_OP_breg8               = swift.runtime.DW_OP_breg8
let DW_OP_breg9               = swift.runtime.DW_OP_breg9
let DW_OP_breg10              = swift.runtime.DW_OP_breg10
let DW_OP_breg11              = swift.runtime.DW_OP_breg11
let DW_OP_breg12              = swift.runtime.DW_OP_breg12
let DW_OP_breg13              = swift.runtime.DW_OP_breg13
let DW_OP_breg14              = swift.runtime.DW_OP_breg14
let DW_OP_breg15              = swift.runtime.DW_OP_breg15
let DW_OP_breg16              = swift.runtime.DW_OP_breg16
let DW_OP_breg17              = swift.runtime.DW_OP_breg17
let DW_OP_breg18              = swift.runtime.DW_OP_breg18
let DW_OP_breg19              = swift.runtime.DW_OP_breg19
let DW_OP_breg20              = swift.runtime.DW_OP_breg20
let DW_OP_breg21              = swift.runtime.DW_OP_breg21
let DW_OP_breg22              = swift.runtime.DW_OP_breg22
let DW_OP_breg23              = swift.runtime.DW_OP_breg23
let DW_OP_breg24              = swift.runtime.DW_OP_breg24
let DW_OP_breg25              = swift.runtime.DW_OP_breg25
let DW_OP_breg26              = swift.runtime.DW_OP_breg26
let DW_OP_breg27              = swift.runtime.DW_OP_breg27
let DW_OP_breg28              = swift.runtime.DW_OP_breg28
let DW_OP_breg29              = swift.runtime.DW_OP_breg29
let DW_OP_breg30              = swift.runtime.DW_OP_breg30
let DW_OP_breg31              = swift.runtime.DW_OP_breg31
let DW_OP_regx                = swift.runtime.DW_OP_regx
let DW_OP_fbreg               = swift.runtime.DW_OP_fbreg
let DW_OP_bregx               = swift.runtime.DW_OP_bregx
let DW_OP_piece               = swift.runtime.DW_OP_piece
let DW_OP_deref_size          = swift.runtime.DW_OP_deref_size
let DW_OP_xderef_size         = swift.runtime.DW_OP_xderef_size
let DW_OP_nop                 = swift.runtime.DW_OP_nop
let DW_OP_push_object_address = swift.runtime.DW_OP_push_object_address
let DW_OP_call2               = swift.runtime.DW_OP_call2
let DW_OP_call4               = swift.runtime.DW_OP_call4
let DW_OP_call_ref            = swift.runtime.DW_OP_call_ref
let DW_OP_form_tls_address    = swift.runtime.DW_OP_form_tls_address
let DW_OP_call_frame_cfa      = swift.runtime.DW_OP_call_frame_cfa
let DW_OP_bit_piece           = swift.runtime.DW_OP_bit_piece
let DW_OP_implicit_value      = swift.runtime.DW_OP_implicit_value
let DW_OP_stack_value         = swift.runtime.DW_OP_stack_value
let DW_OP_implicit_pointer    = swift.runtime.DW_OP_implicit_pointer
let DW_OP_addrx               = swift.runtime.DW_OP_addrx
let DW_OP_constx              = swift.runtime.DW_OP_constx
let DW_OP_entry_value         = swift.runtime.DW_OP_entry_value
let DW_OP_const_type          = swift.runtime.DW_OP_const_type
let DW_OP_regval_type         = swift.runtime.DW_OP_regval_type
let DW_OP_deref_type          = swift.runtime.DW_OP_deref_type
let DW_OP_xderef_type         = swift.runtime.DW_OP_xderef_type
let DW_OP_convert             = swift.runtime.DW_OP_convert
let DW_OP_reinterpret         = swift.runtime.DW_OP_reinterpret
let DW_OP_lo_user             = swift.runtime.DW_OP_lo_user
let DW_OP_hi_user             = swift.runtime.DW_OP_hi_user

typealias Dwarf_LNS_Opcode = swift.runtime.Dwarf_LNS_Opcode
typealias Dwarf_LNE_Opcode = swift.runtime.Dwarf_LNE_Opcode
typealias Dwarf_Lhdr_Format = swift.runtime.Dwarf_Lhdr_Format

typealias DWARF32_Lhdr = swift.runtime.DWARF32_Lhdr
typealias DWARF64_Lhdr = swift.runtime.DWARF64_Lhdr

let DW_CFA_advance_loc        = swift.runtime.DW_CFA_advance_loc
let DW_CFA_offset             = swift.runtime.DW_CFA_offset
let DW_CFA_restore            = swift.runtime.DW_CFA_restore
let DW_CFA_nop                = swift.runtime.DW_CFA_nop
let DW_CFA_set_loc            = swift.runtime.DW_CFA_set_loc
let DW_CFA_advance_loc1       = swift.runtime.DW_CFA_advance_loc1
let DW_CFA_advance_loc2       = swift.runtime.DW_CFA_advance_loc2
let DW_CFA_advance_loc4       = swift.runtime.DW_CFA_advance_loc4
let DW_CFA_offset_extended    = swift.runtime.DW_CFA_offset_extended
let DW_CFA_restore_extended   = swift.runtime.DW_CFA_restore_extended
let DW_CFA_undefined          = swift.runtime.DW_CFA_undefined
let DW_CFA_same_value         = swift.runtime.DW_CFA_same_value
let DW_CFA_register           = swift.runtime.DW_CFA_register
let DW_CFA_remember_state     = swift.runtime.DW_CFA_remember_state
let DW_CFA_restore_state      = swift.runtime.DW_CFA_restore_state
let DW_CFA_def_cfa            = swift.runtime.DW_CFA_def_cfa
let DW_CFA_def_cfa_register   = swift.runtime.DW_CFA_def_cfa_register
let DW_CFA_def_cfa_offset     = swift.runtime.DW_CFA_def_cfa_offset
let DW_CFA_def_cfa_expression = swift.runtime.DW_CFA_def_cfa_expression
let DW_CFA_expression         = swift.runtime.DW_CFA_expression
let DW_CFA_offset_extended_sf = swift.runtime.DW_CFA_offset_extended_sf
let DW_CFA_def_cfa_sf         = swift.runtime.DW_CFA_def_cfa_sf
let DW_CFA_def_cfa_offset_sf  = swift.runtime.DW_CFA_def_cfa_offset_sf
let DW_CFA_val_offset         = swift.runtime.DW_CFA_val_offset
let DW_CFA_val_offset_sf      = swift.runtime.DW_CFA_val_offset_sf
let DW_CFA_val_expression     = swift.runtime.DW_CFA_val_expression
let DW_CFA_lo_user            = swift.runtime.DW_CFA_lo_user
let DW_CFA_hi_user            = swift.runtime.DW_CFA_hi_user

typealias Dwarf_RLE_Entry = swift.runtime.Dwarf_RLE_Entry
typealias Dwarf32_CIEHdr  = swift.runtime.Dwarf32_CIEHdr
typealias Dwarf64_CIEHdr  = swift.runtime.Dwarf64_CIEHdr
typealias Dwarf32_FDEHdr  = swift.runtime.Dwarf32_FDEHdr

typealias EHFrameHdr      = swift.runtime.EHFrameHdr
typealias EHFrameEncoding = swift.runtime.EHFrameEncoding

let DW_EH_PE_omit    = swift.runtime.DW_EH_PE_omit
let DW_EH_PE_uleb128 = swift.runtime.DW_EH_PE_uleb128
let DW_EH_PE_udata2  = swift.runtime.DW_EH_PE_udata2
let DW_EH_PE_udata4  = swift.runtime.DW_EH_PE_udata4
let DW_EH_PE_udata8  = swift.runtime.DW_EH_PE_udata8
let DW_EH_PE_sleb128 = swift.runtime.DW_EH_PE_sleb128
let DW_EH_PE_sdata2  = swift.runtime.DW_EH_PE_sdata2
let DW_EH_PE_sdata4  = swift.runtime.DW_EH_PE_sdata4
let DW_EH_PE_sdata8  = swift.runtime.DW_EH_PE_sdata8
let DW_EH_PE_absptr  = swift.runtime.DW_EH_PE_absptr
let DW_EH_PE_pcrel   = swift.runtime.DW_EH_PE_pcrel
let DW_EH_PE_datarel = swift.runtime.DW_EH_PE_datarel

// .. Dwarf specific errors ....................................................

private enum DwarfError: Error {
  case noDebugInformation
  case unsupportedVersion(Int)
  case unknownEHValueEncoding
  case unknownEHOffsetEncoding
  case badAttribute(UInt64)
  case badForm(UInt64)
  case badTag(UInt64)
  case badLength(UInt32)
  case badAddressSize(Int)
  case badLineContentType(UInt64)
  case badString
  case missingAbbrev(UInt64)
  case doubleIndirectForm
  case unknownForm(Dwarf_Form)
  case missingBaseOffset
  case missingAddrSection
  case missingStrSection
  case missingLineStrSection
  case missingStrOffsetsSection
  case missingAddrBase
  case missingStrOffsetsBase
  case missingLocListsBase
  case unspecifiedAddressSize
}

// .. Dwarf utilities for ImageSource ..........................................

extension ImageSource {

  func fetchULEB128(from a: Address) throws -> (Address, UInt64) {
    var addr = a
    var shift = 0
    var value: UInt64 = 0
    while true {
      let byte = try fetch(from: addr, as: UInt8.self)
      addr += 1
      value |= UInt64(byte & 0x7f) << shift
      if (byte & 0x80) == 0 {
        break
      }
      shift += 7
    }

    return (addr, value)
  }

  func fetchSLEB128(from a: Address) throws -> (Address, Int64) {
    var addr = a
    var shift = 0
    var sign: UInt8 = 0
    var value: Int64 = 0

    while true {
      let byte = try fetch(from: addr, as: UInt8.self)
      addr += 1
      value |= Int64(byte & 0x7f) << shift
      shift += 7
      sign = byte & 0x40
      if (byte & 0x80) == 0 {
        break
      }
    }

    if shift < 64 && sign != 0 {
      value |= -(1 << shift)
    }

    return (addr, value)
  }

  func fetchEHValue(from a: Address, with encoding: EHFrameEncoding,
    pc: Address = 0, data: Address = 0, shouldSwap: Bool = false) throws
    -> (Address, UInt64)? {

    func maybeSwap<T: FixedWidthInteger>(_ x: T) -> T {
      if shouldSwap {
        return x.byteSwapped
      }
      return x
    }

    let valueEnc = EHFrameEncoding(encoding & 0x0f)
    var value: UInt64 = 0
    var addr = a

    switch valueEnc {
    case DW_EH_PE_omit:
      return nil
    case DW_EH_PE_uleb128:
      (addr, value) = try fetchULEB128(from: addr)
    case DW_EH_PE_udata2:
      let u2 = maybeSwap(try fetch(from: addr, as: UInt16.self))
      value = UInt64(u2)
      addr += 2
    case DW_EH_PE_udata4:
      let u4 = maybeSwap(try fetch(from: addr, as: UInt32.self))
      value = UInt64(u4)
      addr += 4
    case DW_EH_PE_udata8:
      let u8 = maybeSwap(try fetch(from: addr, as: UInt64.self))
      value = u8
      addr += 8
    case DW_EH_PE_sleb128:
      let (newAddr, newValue) = try fetchSLEB128(from: addr)
      value = UInt64(bitPattern: newValue)
      addr = newAddr
    case DW_EH_PE_sdata2:
      let s2 = maybeSwap(try fetch(from: addr, as: Int16.self))
      value = UInt64(bitPattern: Int64(s2))
      addr += 2
    case DW_EH_PE_sdata4:
      let s4 = maybeSwap(try fetch(from: addr, as: Int32.self))
      value = UInt64(bitPattern: Int64(s4))
      addr += 4
    case DW_EH_PE_sdata8:
      let s8 = maybeSwap(try fetch(from: addr, as: Int64.self))
      value = UInt64(bitPattern: s8)
      addr += 8
    default:
      throw DwarfError.unknownEHValueEncoding
    }

    let offsetEnc = EHFrameEncoding(encoding & 0xf0)

    switch offsetEnc {
    case DW_EH_PE_absptr:
      return (addr, value)
    case DW_EH_PE_pcrel:
      return (addr, UInt64(pc) &+ value)
    case DW_EH_PE_datarel:
      return (addr, UInt64(data) &+ value)
    default:
      throw DwarfError.unknownEHOffsetEncoding
    }
  }

  func fetchDwarfLength(from addr: Address) throws
    -> (length: UInt64, isDwarf64: Bool) {

    let len32 = try fetch(from: addr, as: UInt32.self)
    if len32 < 0xfffffff0 {
      return (length: UInt64(len32), isDwarf64: false)
    } else if len32 < 0xffffffff {
      throw DwarfError.badLength(len32)
    } else {
      let len64 = try fetch(from: addr + 4, as: UInt64.self)
      return (length: len64, isDwarf64: true)
    }
  }
}

// .. Dwarf utilities for ImageSourceCursor .....................................

extension ImageSourceCursor {

  mutating func readULEB128() throws -> UInt64 {
    let (next, result) = try source.fetchULEB128(from: pos)
    pos = next
    return result
  }

  mutating func readSLEB128() throws -> Int64 {
    let (next, result) = try source.fetchSLEB128(from: pos)
    pos = next
    return result
  }

  mutating func readEHValue(
    with encoding: EHFrameEncoding,
    pc: Address = 0,
    data: Address = 0,
    shouldSwap: Bool = false
  ) throws -> UInt64? {
    guard let (next, result)
            = try source.fetchEHValue(from: pos,
                                      with: encoding,
                                      pc: pc,
                                      data: data,
                                      shouldSwap: shouldSwap) else {
      return nil
    }

    pos = next
    return result
  }

  mutating func readDwarfLength() throws -> (length: UInt64, isDwarf64: Bool) {
    let result = try source.fetchDwarfLength(from: pos)
    pos += result.isDwarf64 ? 12 : 4
    return result
  }

}

// .. DwarfReader ...............................................................

enum DwarfSection {
  case debugAbbrev
  case debugAddr
  case debugARanges
  case debugFrame
  case debugInfo
  case debugLine
  case debugLineStr
  case debugLoc
  case debugLocLists
  case debugMacInfo
  case debugMacro
  case debugNames
  case debugPubNames
  case debugPubTypes
  case debugRanges
  case debugRngLists
  case debugStr
  case debugStrOffsets
  case debugSup
  case debugTypes
  case debugCuIndex
  case debugTuIndex
}

protocol DwarfSource {

  func getDwarfSection(_ section: DwarfSection) -> ImageSource?

}

struct DwarfReader<S: DwarfSource & AnyObject> {

  typealias Source = S
  typealias Address = UInt64
  typealias Size = UInt64
  struct Bounds {
    var base: Address
    var size: Size
    var end: Address { return base + size }
  }

  unowned var source: Source

  struct AbbrevInfo {
    var tag: Dwarf_Tag
    var hasChildren: Bool
    var attributes: [(Dwarf_Attribute, Dwarf_Form, Int64?)]
  }

  var infoSection: ImageSource
  var abbrevSection: ImageSource
  var lineSection: ImageSource?
  var addrSection: ImageSource?
  var strSection: ImageSource?
  var lineStrSection: ImageSource?
  var strOffsetsSection: ImageSource?
  var rangesSection: ImageSource?
  var shouldSwap: Bool

  typealias DwarfAbbrev = UInt64

  struct Unit {
    var baseOffset: Address
    var version: Int
    var isDwarf64: Bool
    var unitType: Dwarf_UnitType
    var addressSize: Int
    var abbrevOffset: Address
    var dieBounds: Bounds

    var lowPC: Address?

    var lineBase: UInt64?
    var addrBase: UInt64?
    var strOffsetsBase: UInt64?
    var loclistsBase: UInt64?

    var abbrevs: [DwarfAbbrev: AbbrevInfo]

    var tag: Dwarf_Tag
    var attributes: [Dwarf_Attribute:DwarfValue] = [:]
  }

  var units: [Unit] = []

  var lineNumberInfo: [DwarfLineNumberInfo] = []

  struct RangeListInfo {
    var length: UInt64
    var isDwarf64: Bool
    var version: Int
    var addressSize: Int
    var segmentSelectorSize: Int
    var offsetEntryCount: Int
    var offsetEntryBase: Address
  }

  var rangeListInfo: RangeListInfo?

  @_specialize(kind: full, where S == Elf32Image)
  @_specialize(kind: full, where S == Elf64Image)
  init(source: Source, shouldSwap: Bool = false) throws {
    // ###TODO: This should be optional, because we can have just line number
    //          information.  We should test that, too.
    guard let abbrevSection = source.getDwarfSection(.debugAbbrev),
          let infoSection = source.getDwarfSection(.debugInfo) else {
      throw DwarfError.noDebugInformation
    }

    self.infoSection = infoSection
    self.abbrevSection = abbrevSection

    addrSection = source.getDwarfSection(.debugAddr)
    strSection = source.getDwarfSection(.debugStr)
    lineSection = source.getDwarfSection(.debugLine)
    lineStrSection = source.getDwarfSection(.debugLineStr)
    strOffsetsSection = source.getDwarfSection(.debugStrOffsets)
    rangesSection = source.getDwarfSection(.debugRanges)

    self.source = source
    self.shouldSwap = shouldSwap
    self.lineNumberInfo = try readLineNumberInfo()
    self.units = try readUnits()

    // On DWARF 4 and earlier, we need to fix up a couple of things in the
    // line number info; these are explicitly included in DWARF 5 so that
    // we can strip everything except line number information.
    for n in 0..<lineNumberInfo.count {
      if lineNumberInfo[n].version >= 5 {
        continue
      }

      for unit in self.units {
        if let lineBase = unit.lineBase,
           lineNumberInfo[n].baseOffset == lineBase {
          var filename = "<unknown>"
          if let nameVal = unit.attributes[.DW_AT_name],
             case let .string(theName) = nameVal {
            filename = theName
          }
          var dirname = "."
          if let dirVal = unit.attributes[.DW_AT_comp_dir],
             case let .string(theDir) = dirVal {
            dirname = theDir
          }

          lineNumberInfo[n].directories[0] = dirname
          lineNumberInfo[n].files[0] = DwarfFileInfo(
            path: filename,
            directoryIndex: 0,
            timestamp: nil,
            size: nil,
            md5sum: nil
          )
          lineNumberInfo[n].addressSize = unit.addressSize
          break
        }
      }
    }
  }

  private func maybeSwap<T: FixedWidthInteger>(_ x: T) -> T {
    if shouldSwap {
      return x.byteSwapped
    } else {
      return x
    }
  }

  private func readUnits() throws -> [Unit] {
    let end = infoSection.bytes.count
    var units: [Unit] = []
    var cursor = ImageSourceCursor(source: infoSection)

    while cursor.pos < end {
      // See 7.5.1.1 Full and Partial Compilation Unit Headers
      let base = cursor.pos

      // .1 unit_length
      let (length, dwarf64) = try cursor.readDwarfLength()
      let next = cursor.pos + length

      // .2 version
      let version = Int(maybeSwap(try cursor.read(as: Dwarf_Half.self)))

      if version < 3 || version > 5 {
        throw DwarfError.unsupportedVersion(version)
      }

      var unitType: Dwarf_UnitType = .DW_UT_unknown
      let addressSize: Int
      let abbrevOffset: Address
      let dieBounds: Bounds

      if dwarf64 {
        if version >= 3 && version <= 4 {
          // .3 debug_abbrev_offset
          abbrevOffset = Address(maybeSwap(try cursor.read(as: Dwarf_Xword.self)))

          // .4 address_size
          addressSize = Int(try cursor.read(as: Dwarf_Byte.self))
        } else if version == 5 {
          // .3 unit_type
          unitType = try cursor.read(as: Dwarf_UnitType.self)

          // .4 address_size
          addressSize = Int(try cursor.read(as: Dwarf_Byte.self))

          // .5 debug_abbrev_offset
          abbrevOffset = Address(maybeSwap(try cursor.read(as: Dwarf_Xword.self)))
        } else {
          throw DwarfError.unsupportedVersion(version)
        }

        dieBounds = Bounds(base: cursor.pos, size: next - cursor.pos)
      } else {
        if version >= 3 && version <= 4 {
          // .3 debug_abbrev_offset
          abbrevOffset = Address(maybeSwap(try cursor.read(as: Dwarf_Word.self)))

          // .4 address_size
          addressSize = Int(try cursor.read(as: Dwarf_Byte.self))
        } else if version == 5 {
          // .3 unit_type
          unitType = try cursor.read(as: Dwarf_UnitType.self)

          // .4 address_size
          addressSize = Int(try cursor.read(as: Dwarf_Byte.self))

          // .5 debug_abbrev_offset
          abbrevOffset = Address(maybeSwap(try cursor.read(as: Dwarf_Word.self)))
        } else {
          throw DwarfError.unsupportedVersion(version)
        }

        dieBounds = Bounds(base: cursor.pos, size: next - cursor.pos)
      }

      if unitType == .DW_UT_skeleton || unitType == .DW_UT_split_compile {
        // .6 dwo_id
        let _ = try cursor.read(as: UInt64.self)
      } else if unitType == .DW_UT_type || unitType == .DW_UT_split_type {
        // .6 type_signature
        let _ = try cursor.read(as: UInt64.self)

        // .7 type_offset
        if dwarf64 {
          let _ = try cursor.read(as: UInt64.self)
        } else {
          let _ = try cursor.read(as: UInt32.self)
        }
      }

      let abbrevs = try readAbbrevs(at: abbrevOffset)

      let abbrev = try cursor.readULEB128()

      guard let abbrevInfo = abbrevs[abbrev] else {
        throw DwarfError.missingAbbrev(abbrev)
      }
      let tag = abbrevInfo.tag

      var unit = Unit(baseOffset: base,
                           version: Int(version),
                           isDwarf64: dwarf64,
                           unitType: unitType,
                           addressSize: Int(addressSize),
                           abbrevOffset: abbrevOffset,
                           dieBounds: dieBounds,
                           abbrevs: abbrevs,
                           tag: tag)

      let attrPos = cursor.pos
      let firstPass = try readDieAttributes(
        at: &cursor,
        unit: unit,
        abbrevInfo: abbrevInfo,
        shouldFetchIndirect: false
      )

      if let value = firstPass[.DW_AT_addr_base],
         case let .sectionOffset(offset) = value {
        unit.addrBase = offset
      }
      if let value = firstPass[.DW_AT_str_offsets_base],
         case let .sectionOffset(offset) = value {
        unit.strOffsetsBase = offset
      }
      if let value = firstPass[.DW_AT_loclists_base],
         case let .sectionOffset(offset) = value {
        unit.loclistsBase = offset
      }
      if let value = firstPass[.DW_AT_stmt_list],
         case let .sectionOffset(offset) = value {
        unit.lineBase = offset
      }
      if let value = firstPass[.DW_AT_low_pc],
         case let .address(lowPC) = value {
        unit.lowPC = lowPC
      }

      // Re-read the attributes, with indirect fetching enabled;
      // we can't do this in one step because attributes might be using
      // indirections based on the base attributes, and those can come
      // after the data needed to decode them.
      cursor.pos = attrPos

      let attributes = try readDieAttributes(
        at: &cursor,
        unit: unit,
        abbrevInfo: abbrevInfo,
        shouldFetchIndirect: true
      )

      unit.attributes = attributes

      units.append(unit)

      cursor.pos = next
    }

    return units
  }

  private func readLineNumberInfo() throws -> [DwarfLineNumberInfo] {
    guard let lineSection = lineSection else {
      return []
    }

    let end = lineSection.bytes.count
    var result: [DwarfLineNumberInfo] = []
    var cursor = ImageSourceCursor(source: lineSection, offset: 0)

    while cursor.pos < end {
      // 6.2.4 The Line Number Program Header

      // .1 unit_length
      let baseOffset = cursor.pos
      let (length, dwarf64) = try cursor.readDwarfLength()
      if length == 0 {
        break
      }

      let nextOffset = cursor.pos + length

      // .2 version
      let version = Int(maybeSwap(try cursor.read(as: Dwarf_Half.self)))

      if version < 3 || version > 5 {
        cursor.pos = nextOffset
        continue
      }

      var addressSize: Int? = nil
      var segmentSelectorSize: Int? = nil

      if version == 5 {
        // .3 address_size
        addressSize = Int(try cursor.read(as: Dwarf_Byte.self))

        // .4 segment_selector_size
        segmentSelectorSize = Int(try cursor.read(as: Dwarf_Byte.self))
      }

      // .5 header_length
      let headerLength: UInt64
      if dwarf64 {
        headerLength = maybeSwap(try cursor.read(as: Dwarf_Xword.self))
      } else {
        headerLength = UInt64(maybeSwap(try cursor.read(as: Dwarf_Word.self)))
      }

      // .6 minimum_instruction_length
      let minimumInstructionLength = UInt(try cursor.read(as: Dwarf_Byte.self))

      // .7 maximum_operations_per_instruction
      let maximumOpsPerInstruction = UInt(try cursor.read(as: Dwarf_Byte.self))

      // .8 default_is_stmt
      let defaultIsStmt = try cursor.read(as: Dwarf_Byte.self) != 0

      // .9 line_base
      let lineBase = try cursor.read(as: Dwarf_Sbyte.self)

      // .10 line_range
      let lineRange = try cursor.read(as: Dwarf_Byte.self)

      // .11 opcode_base
      let opcodeBase = try cursor.read(as: Dwarf_Byte.self)

      // .12 standard_opcode_lengths
      var standardOpcodeLengths: [UInt64] = [0]
      for _ in 1..<Int(opcodeBase) {
        let length = try cursor.readULEB128()
        standardOpcodeLengths.append(length)
      }

      var dirNames: [String] = []
      var fileInfo: [DwarfFileInfo] = []

      if version == 3 || version == 4 {
        // .11 include_directories

        // Prior to version 5, the compilation directory is not included; put
        // a placeholder here for now, which we'll fix later.
        dirNames.append(".")

        while true {
          guard let path = try cursor.readString() else {
            throw DwarfError.badString
          }

          if path == "" {
            break
          }

          dirNames.append(path)
        }

        // .12 file_names

        // Prior to version 5, the compilation unit's filename is not included;
        // put a placeholder here for now, which we'll fix up later.
        fileInfo.append(DwarfFileInfo(
                          path: "<unknown>",
                          directoryIndex: 0,
                          timestamp: nil,
                          size: nil,
                          md5sum: nil))

        while true {
          guard let path = try cursor.readString() else {
            throw DwarfError.badString
          }

          if path == "" {
            break
          }

          let dirIndex = try cursor.readULEB128()
          let timestamp = try cursor.readULEB128()
          let size = try cursor.readULEB128()

          fileInfo.append(DwarfFileInfo(
                            path: path,
                            directoryIndex: Int(dirIndex),
                            timestamp: timestamp != 0 ? Int(timestamp) : nil,
                            size: size != 0 ? size : nil,
                            md5sum: nil))
        }
      } else if version == 5 {
        // .13/.14 directory_entry_format
        var dirEntryFormat: [(Dwarf_Lhdr_Format, Dwarf_Form)] = []
        let dirEntryFormatCount = Int(try cursor.read(as: Dwarf_Byte.self))
        for _ in 0..<dirEntryFormatCount {
          let rawType = try cursor.readULEB128()
          let rawForm = try cursor.readULEB128()

          guard let halfType = Dwarf_Half(exactly: rawType),
                let type = Dwarf_Lhdr_Format(rawValue: halfType) else {
            throw DwarfError.badLineContentType(rawType)
          }
          guard let byteForm = Dwarf_Byte(exactly: rawForm),
                let form = Dwarf_Form(rawValue: byteForm) else {
            throw DwarfError.badForm(rawForm)
          }

          dirEntryFormat.append((type, form))
        }

        // .15 directories_count
        let dirCount = Int(try cursor.readULEB128())

        // .16 directories
        for _ in 0..<dirCount {
          var attributes: [Dwarf_Lhdr_Format: DwarfValue] = [:]
          for (type, form) in dirEntryFormat {
            attributes[type] = try read(form: form,
                                        at: &cursor,
                                        addressSize: addressSize ?? 4,
                                        isDwarf64: dwarf64,
                                        unit: nil,
                                        shouldFetchIndirect: true)
          }

          if let pathVal = attributes[.DW_LNCT_path],
             case let .string(path) = pathVal {
            dirNames.append(path)
          } else {
            dirNames.append("<unknown>")
          }
        }

        // .17/.18 file_name_entry_format
        var fileEntryFormat: [(Dwarf_Lhdr_Format, Dwarf_Form)] = []
        let fileEntryFormatCount = Int(try cursor.read(as: Dwarf_Byte.self))
        for _ in 0..<fileEntryFormatCount {
          let rawType = try cursor.readULEB128()
          let rawForm = try cursor.readULEB128()

          guard let halfType = Dwarf_Half(exactly: rawType),
                let type = Dwarf_Lhdr_Format(rawValue: halfType) else {
            throw DwarfError.badLineContentType(rawType)
          }
          guard let byteForm = Dwarf_Byte(exactly: rawForm),
                let form = Dwarf_Form(rawValue: byteForm) else {
            throw DwarfError.badForm(rawForm)
          }

          fileEntryFormat.append((type, form))
        }

        // .19 file_names_count
        let fileCount = Int(try cursor.readULEB128())

        // .20 file_names
        for _ in 0..<fileCount {
          var attributes: [Dwarf_Lhdr_Format: DwarfValue] = [:]
          for (type, form) in fileEntryFormat {
            attributes[type] = try read(form: form,
                                        at: &cursor,
                                        addressSize: addressSize ?? 4,
                                        isDwarf64: dwarf64,
                                        unit: nil,
                                        shouldFetchIndirect: true)
          }

          let path: String
          if let pathVal = attributes[.DW_LNCT_path],
             case let .string(thePath) = pathVal {
            path = thePath
          } else {
            path = "<unknown>"
          }

          let dirIndex = attributes[.DW_LNCT_directory_index]?.intValue()
          let timestamp = attributes[.DW_LNCT_timestamp]?.intValue()
          let size = attributes[.DW_LNCT_size]?.uint64Value()
          let md5sum: [UInt8]?
          if let md5sumVal = attributes[.DW_LNCT_MD5],
             case let .data(theSum) = md5sumVal {
            md5sum = theSum
          } else {
            md5sum = nil
          }

          fileInfo.append(DwarfFileInfo(
                            path: path,
                            directoryIndex: dirIndex,
                            timestamp: timestamp,
                            size: size,
                            md5sum: md5sum))
        }
      }

      // The actual program comes next
      let program = cursor.source[cursor.pos..<nextOffset]
      cursor.pos = nextOffset

      result.append(DwarfLineNumberInfo(
                      baseOffset: baseOffset,
                      version: version,
                      addressSize: addressSize,
                      selectorSize: segmentSelectorSize,
                      headerLength: headerLength,
                      minimumInstructionLength: minimumInstructionLength,
                      maximumOpsPerInstruction: maximumOpsPerInstruction,
                      defaultIsStmt: defaultIsStmt,
                      lineBase: lineBase,
                      lineRange: lineRange,
                      opcodeBase: opcodeBase,
                      standardOpcodeLengths: standardOpcodeLengths,
                      directories: dirNames,
                      files: fileInfo,
                      program: program,
                      shouldSwap: shouldSwap
                    ))
    }

    return result
  }

  private func readAbbrevs(
    at offset: UInt64
  ) throws -> [DwarfAbbrev: AbbrevInfo] {
    var abbrevs: [DwarfAbbrev: AbbrevInfo] = [:]
    var cursor = ImageSourceCursor(source: abbrevSection, offset: offset)
    while true {
      let abbrev = try cursor.readULEB128()

      if abbrev == 0 {
        break
      }

      let rawTag = try cursor.readULEB128()

      guard let tag = Dwarf_Tag(rawValue: rawTag) else {
        throw DwarfError.badTag(rawTag)
      }

      let children = try cursor.read(as: Dwarf_ChildDetermination.self)

      // Fetch attributes
      var attributes: [(Dwarf_Attribute, Dwarf_Form, Int64?)] = []
      while true {
        let rawAttr = try cursor.readULEB128()
        let rawForm = try cursor.readULEB128()

        if rawAttr == 0 && rawForm == 0 {
          break
        }

        guard let attr = Dwarf_Attribute(rawValue: UInt32(rawAttr)) else {
          throw DwarfError.badAttribute(rawAttr)
        }
        guard let form = Dwarf_Form(rawValue: Dwarf_Byte(rawForm)) else {
          throw DwarfError.badForm(rawForm)
        }

        if form == .DW_FORM_implicit_const {
          let value = try cursor.readSLEB128()
          attributes.append((attr, form, value))
        } else {
          attributes.append((attr, form, nil))
        }
      }

      abbrevs[abbrev] = AbbrevInfo(tag: tag,
                                     hasChildren: children != .DW_CHILDREN_no,
                                     attributes: attributes)
    }

    return abbrevs
  }

  enum DwarfValue {
    case flag(Bool)
    case string(String)
    case address(UInt64)
    case integer(Int)
    case unsignedInt8(UInt8)
    case unsignedInt16(UInt16)
    case unsignedInt32(UInt32)
    case signedInt64(Int64)
    case unsignedInt64(UInt64)
    case dieOffset(UInt64)
    case data([UInt8])
    case expression([UInt8])
    case locationList(UInt64)
    case rangeList(UInt64)
    case sectionOffset(UInt64)
    case reference(UInt64)
    case signature([UInt8])
    case supplementaryReference(UInt64)
    case supplementaryString(UInt64)
    case indirectAddress(UInt64)
    case stringFromStrTab(UInt64)
    case stringFromLineStrTab(UInt64)
    case stringViaStrOffsets(UInt64)

    func uint64Value() -> UInt64? {
      switch self {
        case let .unsignedInt8(value): return UInt64(value)
        case let .unsignedInt16(value): return UInt64(value)
        case let .unsignedInt32(value): return UInt64(value)
        case let .unsignedInt64(value): return value
        default:
          return nil
      }
    }

    func intValue() -> Int? {
      switch self {
        case let .unsignedInt8(value): return Int(value)
        case let .unsignedInt16(value): return Int(value)
        case let .unsignedInt32(value): return Int(value)
        case let .unsignedInt64(value): return Int(value)
        default:
          return nil
      }
    }
  }

  private func threeByteToOffset(_ bytes: (UInt8, UInt8, UInt8)) -> UInt64 {
    let offset: UInt64
    #if _endian(big)
    if shouldSwap {
      offset = UInt64(bytes.0) | UInt64(bytes.1) << 8 | UInt64(bytes.2) << 16
    } else {
      offset = UInt64(bytes.2) | UInt64(bytes.1) << 8 | UInt64(bytes.0) << 16
    }
    #else
    if shouldSwap {
      offset = UInt64(bytes.2) | UInt64(bytes.1) << 8 | UInt64(bytes.0) << 16
    } else {
      offset = UInt64(bytes.0) | UInt64(bytes.1) << 8 | UInt64(bytes.2) << 16
    }
    #endif
    return offset
  }

  private func read(form theForm: Dwarf_Form,
                    at cursor: inout ImageSourceCursor,
                    addressSize: Int, isDwarf64: Bool,
                    unit: Unit?,
                    shouldFetchIndirect: Bool,
                    constantValue: Int64? = nil) throws -> DwarfValue {
    let form: Dwarf_Form
    if theForm == .DW_FORM_indirect {
      let rawForm = try cursor.readULEB128()
      guard let theForm = Dwarf_Form(rawValue: Dwarf_Byte(rawForm)) else {
        throw DwarfError.badForm(rawForm)
      }
      form = theForm
    } else {
      form = theForm
    }

    switch form {
      case .DW_FORM_implicit_const:
        return .signedInt64(constantValue!)

      case .DW_FORM_addr:
        let address: UInt64
        switch addressSize {
          case 4:
            address = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
          case 8:
            address = maybeSwap(try cursor.read(as: UInt64.self))
          default:
            throw DwarfError.badAddressSize(addressSize)
        }
        return .address(address)
      case .DW_FORM_addrx, .DW_FORM_addrx1, .DW_FORM_addrx2,
           .DW_FORM_addrx3, .DW_FORM_addrx4:
        guard let addrSection = addrSection else {
          throw DwarfError.missingAddrSection
        }

        let ndx: UInt64
        switch form {
          case .DW_FORM_addrx:
            ndx = try cursor.readULEB128()
          case .DW_FORM_addrx1:
            ndx = UInt64(try cursor.read(as: UInt8.self))
          case .DW_FORM_addrx2:
            ndx = UInt64(maybeSwap(try cursor.read(as: UInt16.self)))
          case .DW_FORM_addrx3:
            let bytes = try cursor.read(as: (UInt8, UInt8, UInt8).self)
            ndx = threeByteToOffset(bytes)
          case .DW_FORM_addrx4:
            ndx = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
          default:
            fatalError("unreachable")
        }

        if !shouldFetchIndirect {
          return .indirectAddress(ndx)
        } else {
          guard let addrBase = unit?.addrBase else {
            throw DwarfError.missingAddrBase
          }

          let address: UInt64
          switch addressSize {
            case 4:
              address = UInt64(maybeSwap(
                                 try addrSection.fetch(from: ndx * 4 + addrBase,
                                                       as: UInt32.self)))
            case 8:
              address = maybeSwap(try addrSection.fetch(from: ndx * 8 + addrBase,
                                                        as: UInt64.self))
            default:
              throw DwarfError.badAddressSize(addressSize)
          }
          return .address(address)
        }
      case .DW_FORM_block:
        let length = try cursor.readULEB128()
        let bytes = try cursor.read(count: Int(length), as: UInt8.self)
        return .data(bytes)
      case .DW_FORM_block1:
        let length = try cursor.read(as: UInt8.self)
        let bytes = try cursor.read(count: Int(length), as: UInt8.self)
        return .data(bytes)
      case .DW_FORM_block2:
        let length = maybeSwap(try cursor.read(as: UInt16.self))
        let bytes = try cursor.read(count: Int(length), as: UInt8.self)
        return .data(bytes)
      case .DW_FORM_block4:
        let length = maybeSwap(try cursor.read(as: UInt32.self))
        let bytes = try cursor.read(count: Int(length), as: UInt8.self)
        return .data(bytes)

      case .DW_FORM_sdata:
        let data = try cursor.readSLEB128()
        return .signedInt64(data)

      case .DW_FORM_udata:
        let data = try cursor.readULEB128()
        return .unsignedInt64(data)

      case .DW_FORM_data1:
        let data = try cursor.read(as: UInt8.self)
        return .unsignedInt8(data)

      case .DW_FORM_data2:
        let data = maybeSwap(try cursor.read(as: UInt16.self))
        return .unsignedInt16(data)

      case .DW_FORM_data4:
        let data = maybeSwap(try cursor.read(as: UInt32.self))
        return .unsignedInt32(data)

      case .DW_FORM_data8:
        let data = maybeSwap(try cursor.read(as: UInt64.self))
        return .unsignedInt64(data)

      case .DW_FORM_data16:
        let data = try cursor.read(count: 16, as: UInt8.self)
        return .data(data)

      case .DW_FORM_exprloc:
        let length = try cursor.readULEB128()
        let bytes = try cursor.read(count: Int(length), as: UInt8.self)
        return .expression(bytes)

      case .DW_FORM_flag:
        let flag = try cursor.read(as: UInt8.self)
        return .flag(flag != 0)

      case .DW_FORM_flag_present:
        return .flag(true)

      case .DW_FORM_loclistx:
        let offset = try cursor.readULEB128()
        return .locationList(offset)

      case .DW_FORM_sec_offset:
        let offset: UInt64
        if isDwarf64 {
          offset = maybeSwap(try cursor.read(as: UInt64.self))
        } else {
          offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
        }
        return .sectionOffset(offset)

      case .DW_FORM_rnglistx:
        let offset = try cursor.readULEB128()
        return .rangeList(offset)

      case .DW_FORM_ref1, .DW_FORM_ref2, .DW_FORM_ref4, .DW_FORM_ref8,
           .DW_FORM_ref_udata:
        guard let baseOffset = unit?.baseOffset else {
          throw DwarfError.missingBaseOffset
        }

        let offset: Address
        switch form {
          case .DW_FORM_ref1:
            offset = UInt64(try cursor.read(as: UInt8.self))
          case .DW_FORM_ref2:
            offset = UInt64(maybeSwap(try cursor.read(as: UInt16.self)))
          case .DW_FORM_ref4:
            offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
          case .DW_FORM_ref8:
            offset = maybeSwap(try cursor.read(as: UInt64.self))
          case .DW_FORM_ref_udata:
            offset = try cursor.readULEB128()
          default:
            fatalError("unreachable")
        }
        return .reference(offset + baseOffset)

      case .DW_FORM_ref_addr:
        let offset: UInt64
        if isDwarf64 {
          offset = maybeSwap(try cursor.read(as: UInt64.self))
        } else {
          offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
        }
        return .reference(offset)

      case .DW_FORM_ref_sig8:
        let signature = try cursor.read(count: 8, as: UInt8.self)
        return .signature(signature)

      case .DW_FORM_ref_sup4:
        let offset = maybeSwap(try cursor.read(as: UInt32.self))
        return .supplementaryReference(Address(offset))

      case .DW_FORM_ref_sup8:
        let offset = maybeSwap(try cursor.read(as: UInt64.self))
        return .supplementaryReference(Address(offset))

      case .DW_FORM_string:
        guard let string = try cursor.readString() else {
          throw DwarfError.badString
        }
        return .string(string)

      case .DW_FORM_strp:
        guard let strSection = strSection else {
          throw DwarfError.missingStrSection
        }

        let offset: UInt64
        if isDwarf64 {
          offset = maybeSwap(try cursor.read(as: UInt64.self))
        } else {
          offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
        }

        if !shouldFetchIndirect {
          return .stringFromStrTab(offset)
        } else {
          guard let string = try strSection.fetchString(from: offset) else {
            throw DwarfError.badString
          }
          return .string(string)
        }

      case .DW_FORM_strp_sup:
        let offset: UInt64
        if isDwarf64 {
          offset = maybeSwap(try cursor.read(as: UInt64.self))
        } else {
          offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
        }
        return .supplementaryString(offset)

      case .DW_FORM_line_strp:
        guard let lineStrSection = lineStrSection else {
          throw DwarfError.missingLineStrSection
        }

        let offset: UInt64
        if isDwarf64 {
          offset = maybeSwap(try cursor.read(as: UInt64.self))
        } else {
          offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
        }

        if !shouldFetchIndirect {
          return .stringFromLineStrTab(offset)
        } else {
          guard let string = try lineStrSection.fetchString(from: offset) else {
            throw DwarfError.badString
          }
          return .string(string)
        }

      case .DW_FORM_strx,
           .DW_FORM_strx1, .DW_FORM_strx2, .DW_FORM_strx3,.DW_FORM_strx4:
        guard let strOffsetsSection = strOffsetsSection else {
          throw DwarfError.missingStrOffsetsSection
        }
        guard let strSection = strSection else {
          throw DwarfError.missingStrSection
        }

        let offset: UInt64
        switch form {
          case .DW_FORM_strx:
            offset = try cursor.readULEB128()
          case .DW_FORM_strx1:
            offset = UInt64(try cursor.read(as: UInt8.self))
          case .DW_FORM_strx2:
            offset = UInt64(maybeSwap(try cursor.read(as: UInt16.self)))
          case .DW_FORM_strx3:
            let bytes = try cursor.read(as: (UInt8, UInt8, UInt8).self)
            offset = threeByteToOffset(bytes)
          case .DW_FORM_strx4:
            offset = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
          default:
            fatalError("unreachable")
        }

        if !shouldFetchIndirect {
          return .stringViaStrOffsets(offset)
        } else {
          guard let strBase = unit?.strOffsetsBase else {
            throw DwarfError.missingStrOffsetsBase
          }

          let actualOffset: UInt64
          if isDwarf64 {
            actualOffset = maybeSwap(try strOffsetsSection.fetch(
                                       from: offset * 8 + strBase,
                                       as: UInt64.self))
          } else {
            actualOffset = UInt64(maybeSwap(try strOffsetsSection.fetch(
                                              from: offset * 4 + strBase,
                                              as: UInt32.self)))
          }

          guard let string = try strSection.fetchString(from: actualOffset)
          else {
            throw DwarfError.badString
          }
          return .string(string)
        }

      case .DW_FORM_indirect:
        // We should have handled this already
        throw DwarfError.doubleIndirectForm
      default:
        throw DwarfError.unknownForm(theForm)
    }
  }

  private func readDieAttributes(
    at cursor: inout ImageSourceCursor,
    unit: Unit,
    abbrevInfo: AbbrevInfo,
    shouldFetchIndirect: Bool
  ) throws -> [Dwarf_Attribute:DwarfValue] {
    var attributes: [Dwarf_Attribute:DwarfValue] = [:]

    for (attribute, form, constantValue) in abbrevInfo.attributes {
      attributes[attribute] = try read(form: form,
                                       at: &cursor,
                                       addressSize: unit.addressSize,
                                       isDwarf64: unit.isDwarf64,
                                       unit: unit,
                                       shouldFetchIndirect: shouldFetchIndirect,
                                       constantValue: constantValue)
    }

    return attributes
  }

  struct CallSiteInfo {
    var depth: Int
    var rawName: String?
    var name: String?
    var lowPC: Address
    var highPC: Address
    var filename: String
    var line: Int
    var column: Int
  }

  private func buildCallSiteInfo(
    depth: Int,
    unit: Unit,
    attributes: [Dwarf_Attribute:DwarfValue],
    _ fn: (CallSiteInfo) -> ()
  ) throws {
    guard let abstractOriginVal = attributes[.DW_AT_abstract_origin],
          let callFile = attributes[.DW_AT_call_file]?.uint64Value(),
          let callLine = attributes[.DW_AT_call_line]?.uint64Value(),
          let callColumn = attributes[.DW_AT_call_column]?.uint64Value(),
          case let .reference(abstractOrigin) = abstractOriginVal else {
      return
    }

    var cursor = ImageSourceCursor(source: infoSection,
                                   offset: abstractOrigin)
    var abbrev = try cursor.readULEB128()
    if abbrev == 0 {
      return
    }

    guard let abbrevInfo = unit.abbrevs[abbrev] else {
      throw DwarfError.missingAbbrev(abbrev)
    }

    let tag = abbrevInfo.tag

    if tag != .DW_TAG_subprogram {
      return
    }

    var refAttrs = try readDieAttributes(
      at: &cursor,
      unit: unit,
      abbrevInfo: abbrevInfo,
      shouldFetchIndirect: true
    )

    if let specificationVal = refAttrs[.DW_AT_specification],
       case let .reference(specification) = specificationVal {
      cursor = ImageSourceCursor(source: infoSection,
                                 offset: specification)
      abbrev = try cursor.readULEB128()
      if abbrev == 0 {
        return
      }

      guard let abbrevInfo = unit.abbrevs[abbrev] else {
        throw DwarfError.missingAbbrev(abbrev)
      }

      let tag = abbrevInfo.tag
      if tag != .DW_TAG_subprogram {
        return
      }

      refAttrs = try readDieAttributes(
        at: &cursor,
        unit: unit,
        abbrevInfo: abbrevInfo,
        shouldFetchIndirect: true
      )
    }

    var name: String? = nil
    var rawName: String? = nil

    if let nameVal = refAttrs[.DW_AT_name],
       case let .string(theName) = nameVal {
      name = theName
    }

    if let linkageNameVal = refAttrs[.DW_AT_linkage_name],
       case let .string(theRawName) = linkageNameVal {
      rawName = theRawName
    } else {
      rawName = name
    }

    var filename: String = "<unknown>"
    for info in lineNumberInfo {
      if info.baseOffset == unit.lineBase {
        filename = info.fullPathForFile(index: Int(callFile))
        break
      }
    }

    if let lowPCVal = attributes[.DW_AT_low_pc],
       let highPCVal = attributes[.DW_AT_high_pc],
       case let .address(lowPC) = lowPCVal {
      let highPC: Address
      if case let .address(highPCAddr) = highPCVal {
        highPC = highPCAddr
      } else if let highPCOffset = highPCVal.uint64Value() {
        highPC = lowPC + highPCOffset
      } else {
        return
      }

      fn(CallSiteInfo(
           depth: depth,
           rawName: rawName,
           name: name,
           lowPC: lowPC,
           highPC: highPC,
           filename: filename,
           line: Int(callLine),
           column: Int(callColumn)))
    } else if let rangeVal = attributes[.DW_AT_ranges],
              let rangesSection = rangesSection,
              case let .sectionOffset(offset) = rangeVal,
              unit.version < 5 {
      // We don't support .debug_rnglists at present (which is what we'd
      // have if unit.version is 5 or higher).
      var rangeCursor = ImageSourceCursor(source: rangesSection,
                                          offset: offset)
      var rangeBase: Address = unit.lowPC ?? 0

      while true {
        let beginning: Address
        let ending: Address

        switch unit.addressSize {
          case 4:
            beginning = UInt64(maybeSwap(try rangeCursor.read(as: UInt32.self)))
            ending = UInt64(maybeSwap(try rangeCursor.read(as: UInt32.self)))
            if beginning == 0xffffffff {
              rangeBase = ending
              continue
            }
          case 8:
            beginning = maybeSwap(try rangeCursor.read(as: UInt64.self))
            ending = maybeSwap(try rangeCursor.read(as: UInt64.self))
            if beginning == 0xffffffffffffffff {
              rangeBase = ending
              continue
            }
          default:
            throw DwarfError.badAddressSize(unit.addressSize)
        }

        if beginning == 0 && ending == 0 {
          break
        }

        fn(CallSiteInfo(
             depth: depth,
             rawName: rawName,
             name: name,
             lowPC: beginning + rangeBase,
             highPC: ending + rangeBase,
             filename: filename,
             line: Int(callLine),
             column: Int(callColumn)))
      }
    }
  }

  lazy var inlineCallSites: [CallSiteInfo] = _buildCallSiteList()

  private func _buildCallSiteList() -> [CallSiteInfo] {
    var callSites: [CallSiteInfo] = []

    for unit in units {
      do {
        var cursor = ImageSourceCursor(source: infoSection,
                                       offset: unit.dieBounds.base)
        var depth = 0

        while cursor.pos < unit.dieBounds.end {
          let abbrev = try cursor.readULEB128()

          if abbrev == 0 {
            depth -= 1
            if depth == 0 {
              break
            }
            continue
          }

          guard let abbrevInfo = unit.abbrevs[abbrev] else {
            throw DwarfError.missingAbbrev(abbrev)
          }

          let tag = abbrevInfo.tag

          let attributes = try readDieAttributes(
            at: &cursor,
            unit: unit,
            abbrevInfo: abbrevInfo,
            shouldFetchIndirect: tag == .DW_TAG_inlined_subroutine
          )

          if tag == .DW_TAG_inlined_subroutine {
            try buildCallSiteInfo(depth: depth,
                                  unit: unit,
                                  attributes: attributes) {
              callSites.append($0)
            }
          }

          if abbrevInfo.hasChildren {
            depth += 1
          }
        }
      } catch {
        let name: String
        if let value = unit.attributes[.DW_AT_name],
           case let .string(theName) = value {
          name = theName
        } else {
          name = "<unknown at \(hex(unit.baseOffset))>"
        }
        swift_reportWarning(0,
                            """
                              swift-runtime: warning: unable to fetch inline \
                              frame data for DWARF unit \(name): \(error)
                              """)
      }
    }

    callSites.sort(
      by: { (a, b) in
        a.lowPC < b.lowPC || (a.lowPC == b.lowPC) && a.depth > b.depth
      })

    return callSites
  }

}

struct DwarfFileInfo {
  var path: String
  var directoryIndex: Int?
  var timestamp: Int?
  var size: UInt64?
  var md5sum: [UInt8]?
}

struct DwarfLineNumberState: CustomStringConvertible {
  typealias Address = UInt64

  var address: Address
  var opIndex: UInt
  var file: Int
  var path: String
  var line: Int
  var column: Int
  var isStmt: Bool
  var basicBlock: Bool
  var endSequence: Bool
  var prologueEnd: Bool
  var epilogueBegin: Bool
  var isa: UInt
  var discriminator: UInt

  var description: String {
    var flags: [String] = []
    if isStmt {
      flags.append("is_stmt")
    }
    if basicBlock {
      flags.append("basic_block")
    }
    if endSequence {
      flags.append("end_sequence")
    }
    if prologueEnd {
      flags.append("prologue_end")
    }
    if epilogueBegin {
      flags.append("epilogue_begin")
    }

    let flagsString = flags.joined(separator:" ")

    return """
      \(hex(address)) \(pad(line, 6)) \(pad(column, 6)) \(pad(file, 6)) \
      \(pad(isa, 3)) \(pad(discriminator, 13)) \(flagsString)
      """
  }
}

struct DwarfLineNumberInfo {
  typealias Address = UInt64

  var baseOffset: Address
  var version: Int
  var addressSize: Int?
  var selectorSize: Int?
  var headerLength: UInt64
  var minimumInstructionLength: UInt
  var maximumOpsPerInstruction: UInt
  var defaultIsStmt: Bool
  var lineBase: Int8
  var lineRange: UInt8
  var opcodeBase: UInt8
  var standardOpcodeLengths: [UInt64]
  var directories: [String] = []
  var files: [DwarfFileInfo] = []
  var program: ImageSource
  var shouldSwap: Bool

  /// Compute the full path for a file, given its index in the file table.
  func fullPathForFile(index: Int) -> String {
    if index >= files.count {
      return "<unknown>"
    }

    let info = files[index]
    if info.path.hasPrefix("/") {
      return info.path
    }

    let dirName: String
    if let dirIndex = info.directoryIndex,
       dirIndex < directories.count {
      dirName = directories[dirIndex]
    } else {
      dirName = "<unknown>"
    }

    return "\(dirName)/\(info.path)"
  }

  /// Execute the line number program, calling a closure for every line
  /// table entry.
  mutating func executeProgram(
    line: (DwarfLineNumberState, inout Bool) -> ()
  ) throws {
    let end = program.bytes.count
    var cursor = ImageSourceCursor(source: program)

    func maybeSwap<T: FixedWidthInteger>(_ x: T) -> T {
      if shouldSwap {
        return x.byteSwapped
      }
      return x
    }

    // Table 6.4: Line number program initial state
    let initialState = DwarfLineNumberState(
      address: 0,
      opIndex: 0,
      file: 1,
      path: fullPathForFile(index: 1),
      line: 1,
      column: 0,
      isStmt: defaultIsStmt,
      basicBlock: false,
      endSequence: false,
      prologueEnd: false,
      epilogueBegin: false,
      isa: 0,
      discriminator: 0
    )

    var state = initialState

    // Flag to allow fast exit
    var done = false

    while !done && cursor.pos < end {
      let opcode = try cursor.read(as: Dwarf_LNS_Opcode.self)

      if opcode.rawValue >= opcodeBase {
        // Special opcode
        let adjustedOpcode = UInt(opcode.rawValue - opcodeBase)
        let advance = adjustedOpcode / UInt(lineRange)
        let lineAdvance = adjustedOpcode % UInt(lineRange)
        let instrAdvance
          = (state.opIndex + advance) / maximumOpsPerInstruction
        let newOp = (state.opIndex + advance) % maximumOpsPerInstruction
        state.address += Address(instrAdvance)
        state.opIndex = newOp
        state.line += Int(lineBase) + Int(lineAdvance)

        line(state, &done)

        state.discriminator = 0
        state.basicBlock = false
        state.prologueEnd = false
        state.epilogueBegin = false
      } else if opcode == .DW_LNS_extended {
        // Extended opcode
        let length = try cursor.readULEB128()
        let opcode = try cursor.read(as: Dwarf_LNE_Opcode.self)

        switch opcode {
          case .DW_LNE_end_sequence:
            state.endSequence = true
            line(state, &done)
            state = initialState
          case .DW_LNE_set_address:
            let address: UInt64
            guard let addressSize = addressSize else {
              throw DwarfError.unspecifiedAddressSize
            }
            switch addressSize {
              case 4:
                address = UInt64(maybeSwap(try cursor.read(as: UInt32.self)))
              case 8:
                address = maybeSwap(try cursor.read(as: UInt64.self))
              default:
                throw DwarfError.badAddressSize(addressSize)
            }
            state.address = Address(address)
              case .DW_LNE_define_file:
                guard let path = try cursor.readString() else {
                  throw DwarfError.badString
                }
                let directoryIndex = try cursor.readULEB128()
                let timestamp = try cursor.readULEB128()
                let size = try cursor.readULEB128()
                files.append(DwarfFileInfo(
                               path: path,
                               directoryIndex: Int(directoryIndex),
                               timestamp: timestamp != 0 ? Int(timestamp) : nil,
                               size: size != 0 ? size : nil,
                               md5sum: nil
                             ))
              case .DW_LNE_set_discriminator:
                let discriminator = try cursor.readULEB128()
                state.discriminator = UInt(discriminator)
              default:
                cursor.pos += length - 1
        }
      } else {
        // Standard opcode
        switch opcode {
          case .DW_LNS_copy:
            line(state, &done)
            state.discriminator = 0
            state.basicBlock = false
            state.prologueEnd = false
            state.epilogueBegin = false
          case .DW_LNS_advance_pc:
            let advance = UInt(try cursor.readULEB128())
            let instrAdvance
              = (state.opIndex + advance) / maximumOpsPerInstruction
            let newOp = (state.opIndex + advance) % maximumOpsPerInstruction
            state.address += Address(instrAdvance)
            state.opIndex = newOp
          case .DW_LNS_advance_line:
            let advance = try cursor.readSLEB128()
            state.line += Int(advance)
          case .DW_LNS_set_file:
            let file = Int(try cursor.readULEB128())
            state.file = file
            state.path = fullPathForFile(index: state.file)
          case .DW_LNS_set_column:
            let column = Int(try cursor.readULEB128())
            state.column = column
          case .DW_LNS_negate_stmt:
            state.isStmt = !state.isStmt
          case .DW_LNS_set_basic_block:
            state.basicBlock = true
          case .DW_LNS_const_add_pc:
            let adjustedOpcode = UInt(255 - opcodeBase)
            let advance = adjustedOpcode / UInt(lineRange)
            let instrAdvance
              = (state.opIndex + advance) / maximumOpsPerInstruction
            let newOp = (state.opIndex + advance) % maximumOpsPerInstruction
            state.address += Address(instrAdvance)
            state.opIndex = newOp
          case .DW_LNS_fixed_advance_pc:
            let advance = try cursor.read(as: Dwarf_Half.self)
            state.address += Address(advance)
            state.opIndex = 0
          case .DW_LNS_set_prologue_end:
            state.prologueEnd = true
          case .DW_LNS_set_epilogue_begin:
            state.epilogueBegin = true
          case .DW_LNS_set_isa:
            let isa = UInt(try cursor.readULEB128())
            state.isa = isa
          default:
            // Skip this unknown opcode
            let length = standardOpcodeLengths[Int(opcode.rawValue)]
            for _ in 0..<length {
              _ = try cursor.readULEB128()
            }
        }
      }
    }
  }
}

// .. Testing ..................................................................

@_spi(DwarfTest)
public func testDwarfReaderFor(path: String) -> Bool {
  guard let source = try? ImageSource(path: path) else {
    print("\(path) was not accessible")
    return false
  }

  if let elfImage = try? Elf32Image(source: source) {
    print("\(path) is a 32-bit ELF image")

    var reader: DwarfReader<Elf32Image>
    do {
      reader = try DwarfReader(source: elfImage)
    } catch {
      print("Unable to create reader - \(error)")
      return false
    }

    print("Units:")
    print(reader.units)

    print("Call Sites:")
    print(reader.inlineCallSites)
    return true
  } else if let elfImage = try? Elf64Image(source: source) {
    print("\(path) is a 64-bit ELF image")

    var reader: DwarfReader<Elf64Image>
    do {
      reader = try DwarfReader(source: elfImage)
    } catch {
      print("Unable to create reader - \(error)")
      return false
    }

    print("Units:")
    for unit in reader.units {
      if let value = unit.attributes[.DW_AT_name],
         case let .string(name) = value {
        print("  \(name)")
      } else {
        print("  <unnamed>")
      }
    }

    print("Call Sites:")
    print(reader.inlineCallSites)
    return true
  } else {
    print("\(path) is not an ELF image")
    return false
  }
}

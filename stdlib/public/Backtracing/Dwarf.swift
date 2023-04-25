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

@_implementationOnly import ImageFormats.Dwarf

// .. Dwarf specific errors ....................................................

private enum DwarfError: Error {
  case unknownEHValueEncoding
  case unknownEHOffsetEncoding
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

}

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

internal protocol ElfSym {
  static var symbolSize: Int { get }
  var name: UInt32 { get }
  var info: UInt8 { get }
  var other: UInt8 { get }
  var shndx: UInt16 { get }
  var value: UInt64 { get }
  var size: UInt64 { get }
}

extension Elf32_Sym: ElfSym {
  static var symbolSize: Int { MemoryLayout<Elf32_Sym>.size }
  var name: UInt32 { self.st_name }
  var info: UInt8 { self.st_info }
  var other: UInt8 { self.st_other }
  var shndx: UInt16 { self.st_shndx }
  var value: UInt64 { UInt64(self.st_value) }
  var size: UInt64 { UInt64(self.st_size) }
}

extension Elf64_Sym: ElfSym {
  static var symbolSize: Int { MemoryLayout<Elf64_Sym>.size }
  var name: UInt32 { self.st_name }
  var info: UInt8 { self.st_info }
  var other: UInt8 { self.st_other }
  var shndx: UInt16 { self.st_shndx }
  var value: UInt64 { self.st_value }
  var size: UInt64 { self.st_size }
}

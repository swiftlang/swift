//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

internal protocol ElfEhdr {
  static var symbolSize: Int { get }
  var ident: [UInt8] { get }
  var type: UInt16 { get }
  var machine: UInt16 { get }
  var version: UInt32 { get }
  var entry: UInt64 { get }
  var phoff: UInt64 { get }
  var shoff: UInt64 { get }
  var flags: UInt32 { get }
  var ehsize: UInt16 { get }
  var phentsize: UInt16 { get }
  var phnum: UInt16 { get }
  var shentsize: UInt16 { get }
  var shnum: UInt16 { get }
  var shstrndx: UInt16 { get }
}

extension Elf64_Ehdr: ElfEhdr {
  static var symbolSize: Int { MemoryLayout<Elf64_Ehdr>.size }
  var ident: [UInt8] { return withUnsafeBytes(of: self.e_ident) { Array($0) } }
  var type: UInt16 { self.e_type }
  var machine: UInt16 { self.e_machine }
  var version: UInt32 { self.e_version }
  var entry: UInt64 { self.e_entry }
  var phoff: UInt64 { self.e_phoff }
  var shoff: UInt64 { self.e_shoff }
  var flags: UInt32 { self.e_flags }
  var ehsize: UInt16 { self.e_ehsize }
  var phentsize: UInt16 { self.e_phentsize }
  var phnum: UInt16 { self.e_phnum }
  var shentsize: UInt16 { self.e_shentsize }
  var shnum: UInt16 { self.e_shnum }
  var shstrndx: UInt16 { self.e_shstrndx }
}

extension Elf32_Ehdr: ElfEhdr {
  static var symbolSize: Int { MemoryLayout<Elf32_Ehdr>.size }
  var ident: [UInt8] { return withUnsafeBytes(of: self.e_ident) { Array($0) } }
  var type: UInt16 { self.e_type }
  var machine: UInt16 { self.e_machine }
  var version: UInt32 { self.e_version }
  var entry: UInt64 { UInt64(self.e_entry) }
  var phoff: UInt64 { UInt64(self.e_phoff) }
  var shoff: UInt64 { UInt64(self.e_shoff) }
  var flags: UInt32 { self.e_flags }
  var ehsize: UInt16 { self.e_ehsize }
  var phentsize: UInt16 { self.e_phentsize }
  var phnum: UInt16 { self.e_phnum }
  var shentsize: UInt16 { self.e_shentsize }
  var shnum: UInt16 { self.e_shnum }
  var shstrndx: UInt16 { self.e_shstrndx }
}

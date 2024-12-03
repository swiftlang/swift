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

internal protocol ElfShdr {
  static var symbolSize: Int { get }
  var name: UInt32 { get }
  var type: UInt32 { get }
  var flags: UInt64 { get }
  var addr: UInt64 { get }
  var offset: UInt64 { get }
  var size: UInt64 { get }
  var link: UInt32 { get }
  var info: UInt32 { get }
  var addralign: UInt64 { get }
  var entsize: UInt64 { get }
}

extension Elf32_Shdr: ElfShdr {
  static var symbolSize: Int { MemoryLayout<Elf32_Shdr>.size }
  var name: UInt32 { self.sh_name }
  var type: UInt32 { self.sh_type }
  var flags: UInt64 { UInt64(self.sh_flags) }
  var addr: UInt64 { UInt64(self.sh_addr) }
  var offset: UInt64 { UInt64(self.sh_offset) }
  var size: UInt64 { UInt64(self.sh_size) }
  var link: UInt32 { self.sh_link }
  var info: UInt32 { self.sh_info }
  var addralign: UInt64 { UInt64(self.sh_addralign) }
  var entsize: UInt64 { UInt64(self.sh_entsize) }
}

extension Elf64_Shdr: ElfShdr {
  static var symbolSize: Int { MemoryLayout<Elf64_Shdr>.size }
  var name: UInt32 { self.sh_name }
  var type: UInt32 { self.sh_type }
  var flags: UInt64 { self.sh_flags }
  var addr: UInt64 { self.sh_addr }
  var offset: UInt64 { self.sh_offset }
  var size: UInt64 { self.sh_size }
  var link: UInt32 { self.sh_link }
  var info: UInt32 { self.sh_info }
  var addralign: UInt64 { self.sh_addralign }
  var entsize: UInt64 { self.sh_entsize }
}

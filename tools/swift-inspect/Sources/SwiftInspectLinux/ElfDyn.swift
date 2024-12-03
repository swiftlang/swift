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

protocol ElfDyn {
  static var symbolSize: Int { get }
  var tag: Int64 { get }
  var val: UInt64 { get }
}

extension Elf64_Dyn: ElfDyn {
  static var symbolSize: Int { MemoryLayout<Elf64_Dyn>.size }
  var tag: Int64 { self.d_tag }
  var val: UInt64 { self.d_un.d_val }
}

extension Elf32_Dyn: ElfDyn {
  static var symbolSize: Int { MemoryLayout<Elf32_Dyn>.size }
  var tag: Int64 { Int64(self.d_tag) }
  var val: UInt64 { UInt64(self.d_un.d_val) }
}

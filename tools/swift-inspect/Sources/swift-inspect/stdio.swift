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

enum Std {
  struct File: TextOutputStream {
    var underlying: UnsafeMutablePointer<FILE>

    mutating func write(_ string: String) {
      fputs(string, underlying)
    }
  }
  
  static var err = File(underlying: stderr)
}

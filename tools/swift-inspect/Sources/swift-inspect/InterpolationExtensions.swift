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

import SwiftRemoteMirror

extension DefaultStringInterpolation {
  mutating func appendInterpolation(hex: swift_reflection_ptr_t) {
    appendInterpolation("0x")
    appendInterpolation(String(hex, radix: 16))
  }
}

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension std.string {
  public init(_ string: String) {
    let count = string.count
    self.init(count, value_type(0))
    var string = string
    string.withUTF8 {
      memcpy(UnsafeMutableRawPointer(mutating: UnsafeRawPointer(__dataUnsafe()!)),
             UnsafeMutableRawPointer(mutating: $0.baseAddress), count)
    }
  }
}

extension String {
  public init(cxxString: std.string) {
    self.init(cString: cxxString.__c_strUnsafe())
    withExtendedLifetime(cxxString) {}
  }
}

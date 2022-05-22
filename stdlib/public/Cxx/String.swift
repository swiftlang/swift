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
    self.init()
    for char in string.utf8 {
      self.push_back(value_type(char))
    }
  }
}

extension String {
  public init(cxxString: std.string) {
    self.init(cString: cxxString.c_str())
  }
}

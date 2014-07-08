//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct _Process {
  // Use lazy initialization of static properties to safely initialize the
  // public 'arguments' property on first use.
  static var _arguments: [String] = {
    map(0..<Int(C_ARGC)) { i in 
      if let s = String.fromCStringRepairingIllFormedUTF8(C_ARGV[i]).0 {
          return s
      }
      return ""
    }
  }()

  public var arguments : [String] {
    return _Process._arguments
  }
}

public var Process = _Process()

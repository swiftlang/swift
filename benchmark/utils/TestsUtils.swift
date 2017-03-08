//===--- TestsUtils.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Linux)
import Glibc
#else
import Darwin
#endif

// Linear function shift register.
//
// This is just to drive benchmarks. I don't make any claim about its
// strength. According to Wikipedia, it has the maximal period for a
// 32-bit register.
struct LFSR {
  // Set the register to some seed that I pulled out of a hat.
  var lfsr : UInt32 = 0xb78978e7

  mutating func shift() {
    lfsr = (lfsr >> 1) ^ (UInt32(bitPattern: -Int32((lfsr & 1))) & 0xD0000001)
  }
  mutating func randInt() -> Int64 {
    var result : UInt32 = 0
    for _ in 0..<32 {
      result = (result << 1) | (lfsr & 1)
      shift()
    }
    return Int64(bitPattern: UInt64(result))
  }
}

var lfsrRandomGenerator = LFSR()

// Start the generator from the beginning
public func SRand() {
  lfsrRandomGenerator = LFSR()
}

public func Random() -> Int64 {
  return lfsrRandomGenerator.randInt()
}

public func CheckResults(_ res: Bool, _ message: String = "") {
  if res {
    return
  }
  print(message)
  abort()
}

public func False() -> Bool { return false }

/// This is a dummy protocol to test the speed of our protocol dispatch.
public protocol SomeProtocol { func getValue() -> Int }
struct MyStruct : SomeProtocol {
  init() {}
  func getValue() -> Int { return 1 }
}
public func someProtocolFactory() -> SomeProtocol { return MyStruct() }


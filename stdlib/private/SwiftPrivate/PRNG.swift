//===--- PRNG.swift -------------------------------------------------------===//
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

import SwiftShims

public func rand32() -> UInt32 {
  return _swift_stdlib_cxx11_mt19937()
}

public func rand32(exclusiveUpperBound limit: UInt32) -> UInt32 {
  return _swift_stdlib_cxx11_mt19937_uniform(limit)
}

public func rand64() -> UInt64 {
  return
    (UInt64(_swift_stdlib_cxx11_mt19937()) << 32) |
    UInt64(_swift_stdlib_cxx11_mt19937())
}

public func randInt() -> Int {
#if arch(i386) || arch(arm)
  return Int(Int32(bitPattern: rand32()))
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  return Int(Int64(bitPattern: rand64()))
#else
  fatalError("unimplemented")
#endif
}

public func randArray64(_ count: Int) -> [UInt64] {
  var result = [UInt64](repeating: 0, count: count)
  for i in result.indices {
    result[i] = rand64()
  }
  return result
}

public func randArray(_ count: Int) -> [Int] {
  var result = [Int](repeating: 0, count: count)
  for i in result.indices {
    result[i] = randInt()
  }
  return result
}

public func pickRandom<
  C : RandomAccessCollection
>(_ c: C) -> C.Iterator.Element {
  let i = Int(rand32(exclusiveUpperBound: numericCast(c.count)))
  return c[c.index(c.startIndex, offsetBy: numericCast(i))]
}

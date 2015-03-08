//===--- PRNG.swift -------------------------------------------------------===//
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

import SwiftShims

@inline(never) @semantics("stdlib_binary_only") // Hide the libbsd dependency
public func rand32() -> UInt32 {
  return arc4random()
}

@inline(never) @semantics("stdlib_binary_only") // Hide the libbsd dependency
public func rand32(#exclusiveUpperBound: UInt32) -> UInt32 {
  return arc4random_uniform(exclusiveUpperBound)
}

@inline(never) @semantics("stdlib_binary_only") // Hide the libbsd dependency
public func rand64() -> UInt64 {
  return (UInt64(arc4random()) << 32) | UInt64(arc4random())
}

public func randInt() -> Int {
#if arch(i386) || arch(arm)
  return Int(Int32(bitPattern: rand32()))
#elseif arch(x86_64) || arch(arm64)
  return Int(Int64(bitPattern: rand64()))
#else
  fatalError("unimplemented")
#endif
}

public func randArray64(count: Int) -> _UnitTestArray<UInt64> {
  var result = _UnitTestArray<UInt64>(count: count, repeatedValue: 0)
  for i in indices(result) {
    result[i] = rand64()
  }
  return result
}

public func randArray(count: Int) -> [Int] {
  var result = [Int](count: count, repeatedValue: 0)
  for i in indices(result) {
    result[i] = randInt()
  }
  return result
}

public func pickRandom<
  C : CollectionType where C.Index : RandomAccessIndexType
>(c: C) -> C.Generator.Element {
  let i = Int(rand32(exclusiveUpperBound: numericCast(count(c))))
  return c[advance(c.startIndex, numericCast(i))]
}


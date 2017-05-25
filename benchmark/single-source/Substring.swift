//===--- Substring.swift --------------------------------------------------===//
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

import TestsUtils

// A string that doesn't fit in small string storage and doesn't fit in Latin-1
let longWide = "fὢasὢodὢijὢadὢolὢsjὢalὢsdὢjlὢasὢdfὢijὢliὢsdὢjøὢslὢdiὢalὢiὢ"

@inline(never)
public func run_SubstringFromLongString(_ N: Int) {
  var s = longWide
  s += "!" // ensure the string has a real buffer
  for _ in 1...N*500 {
    blackHole(Substring(s))
  }
}

func create<T : RangeReplaceableCollection, U : Collection>(
  _: T.Type, from source: U
) where T.Iterator.Element == U.Iterator.Element {
  blackHole(T(source))
}

@inline(never)
public func run_SubstringFromLongStringGeneric(_ N: Int) {
  var s = longWide
  s += "!" // ensure the string has a real buffer
  for _ in 1...N*500 {
    create(Substring.self, from: s)
  }
}

@inline(never)
public func run_StringFromLongWholeSubstring(_ N: Int) {
  var s0 = longWide
  s0 += "!" // ensure the string has a real buffer
  let s = Substring(s0)
  for _ in 1...N*500 {
    blackHole(String(s))
  }
}

@inline(never)
public func run_StringFromLongWholeSubstringGeneric(_ N: Int) {
  var s0 = longWide
  s0 += "!" // ensure the string has a real buffer
  let s = Substring(s0)
  for _ in 1...N*500 {
    create(String.self, from: s)
  }
}

private func equivalentWithDistinctBuffers() -> (String, Substring) {
  var s0 = longWide
  withUnsafeMutablePointer(to: &s0) { blackHole($0) }
  s0 += "!"
  
  // These two should be equal but with distinct buffers, both refcounted.
  let a = Substring(s0).dropFirst()
  let b = String(a)
  return (b, a)
}

@inline(never)
public func run_EqualStringSubstring(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(a == b)
  }
}

@inline(never)
public func run_EqualSubstringString(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(b == a)
  }
}

@inline(never)
public func run_EqualSubstringSubstring(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(a == b)
  }
}

@inline(never)
public func run_EqualSubstringSubstringGenericEquatable(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  func check<T>(_ x: T, _ y: T) where T : Equatable {
    blackHole(x == y)
  }
  for _ in 1...N*500 {
    check(a, b)
  }
}

/*
func checkEqual<T, U>(_ x: T, _ y: U)
where T : StringProtocol, U : StringProtocol {
  blackHole(x == y)
}

@inline(never)
public func run_EqualStringSubstringGenericStringProtocol(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkEqual(a, b)
  }
}

@inline(never)
public func run_EqualSubstringStringGenericStringProtocol(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkEqual(b, a)
  }
}

@inline(never)
public func run_EqualSubstringSubstringGenericStringProtocol(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkEqual(a, b)
  }
}
*/

//===----------------------------------------------------------------------===//

/*
@inline(never)
public func run_LessStringSubstring(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(a < b)
  }
}

@inline(never)
public func run_LessSubstringString(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(b < a)
  }
}
*/

@inline(never)
public func run_LessSubstringSubstring(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    blackHole(a < b)
  }
}

@inline(never)
public func run_LessSubstringSubstringGenericComparable(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  func check<T>(_ x: T, _ y: T) where T : Comparable {
    blackHole(x < y)
  }
  for _ in 1...N*500 {
    check(a, b)
  }
}

/*
func checkLess<T, U>(_ x: T, _ y: U)
where T : StringProtocol, U : StringProtocol {
  blackHole(x < y)
}

@inline(never)
public func run_LessStringSubstringGenericStringProtocol(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkLess(a, b)
  }
}

@inline(never)
public func run_LessSubstringStringGenericStringProtocol(_ N: Int) {
  let (a, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkLess(b, a)
  }
}

@inline(never)
public func run_LessSubstringSubstringGenericStringProtocol(_ N: Int) {
  let (_, a) = equivalentWithDistinctBuffers()
  let (_, b) = equivalentWithDistinctBuffers()
  for _ in 1...N*500 {
    checkLess(a, b)
  }
}
*/

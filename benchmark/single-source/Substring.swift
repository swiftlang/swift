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


//===--- CharacterLiteralsSmall.swift -------------------------------------===//
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

// This test tests the performance of Characters initialized from literals that
// fit within the small (63 bits or fewer) representation and can be
// represented as a packed integer.
import TestsUtils

@inline(never)
func makeCharacter_UTF8Length1() -> Character {
  return "a"
}

@inline(never)
func makeCharacter_UTF8Length2() -> Character {
  return "\u{00a9}"
}

@inline(never)
func makeCharacter_UTF8Length3() -> Character {
  return "a\u{0300}"
}

@inline(never)
func makeCharacter_UTF8Length4() -> Character {
  return "\u{00a9}\u{0300}"
}

@inline(never)
func makeCharacter_UTF8Length5() -> Character {
  return "a\u{0300}\u{0301}"
}

@inline(never)
func makeCharacter_UTF8Length6() -> Character {
  return "\u{00a9}\u{0300}\u{0301}"
}

@inline(never)
func makeCharacter_UTF8Length7() -> Character {
  return "a\u{0300}\u{0301}\u{0302}"
}

@inline(never)
func makeCharacter_UTF8Length8() -> Character {
  return "\u{00a9}\u{0300}\u{0301}\u{0302}"
}

public func run_CharacterLiteralsSmall(_ N: Int) {
  for _ in 0...10000 * N {
    _ = makeCharacter_UTF8Length1()
    _ = makeCharacter_UTF8Length2()
    _ = makeCharacter_UTF8Length3()
    _ = makeCharacter_UTF8Length4()
    _ = makeCharacter_UTF8Length5()
    _ = makeCharacter_UTF8Length6()
    _ = makeCharacter_UTF8Length7()
    _ = makeCharacter_UTF8Length8()
  }
}

//===--- CharacterLiteralsLarge.swift -------------------------------------===//
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

// This test tests the performance of Characters initialized from literals
// which don't fit into the small (63-bit) representation and need to allocate
// and retain a StringBuffer.
import TestsUtils

@inline(never)
func makeCharacter_UTF8Length9() -> Character {
  return "a\u{0300}\u{0301}\u{0302}\u{0303}"
}

@inline(never)
func makeCharacter_UTF8Length10() -> Character {
  return "\u{00a9}\u{0300}\u{0301}\u{0302}\u{0303}"
}

@inline(never)
func makeCharacter_UTF8Length11() -> Character {
  return "a\u{0300}\u{0301}\u{0302}\u{0303}\u{0304}"
}

@inline(never)
func makeCharacter_UTF8Length12() -> Character {
  return "\u{00a9}\u{0300}\u{0301}\u{0302}\u{0303}\u{0304}"
}

public func run_CharacterLiteralsLarge(_ N: Int) {
  for _ in 0...10000 * N {
    _ = makeCharacter_UTF8Length9()
    _ = makeCharacter_UTF8Length10()
    _ = makeCharacter_UTF8Length11()
    _ = makeCharacter_UTF8Length12()
  }
}

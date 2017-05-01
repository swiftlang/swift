//===--- StringWalk.swift -------------------------------------------------===//
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

// Test String subscript performance.
//
// Subscript has a slow path that initializes a global variable:
// Swift._cocoaStringSubscript.addressor.  Global optimization would
// normally hoist the initializer outside the inner loop (over
// unicodeScalars), forcing the initializer to be called on each
// lap. However, no that the cocoa code is properly marked "slowPath",
// no hoisting should occur.
import TestsUtils

var count: Int = 0

//
// Helper functionality
//

@inline(never) func countScalars(_ s: String.UnicodeScalarView) {
  for _ in s {
    count += 1
  }
}
@inline(never) func countCharacters(_ s: String.CharacterView) {
  for _ in s {
    count += 1
  }
}
@inline(never) func countScalars_rev(
	_ s: ReversedCollection<String.UnicodeScalarView>
) {
  for _ in s {
    count += 1
  }
}
@inline(never) func countCharacters_rev(
	_ s: ReversedCollection<String.CharacterView>
) {
  for _ in s {
    count += 1
  }
}

//
// Workloads
//
let asciiString =
  "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"
let winter = "🏂☃❅❆❄︎⛄️❄️"
let utf16String =
  winter + "the quick brown fox" + String(winter.characters.reversed())

// A workload that's mostly Latin characters, with occasional emoji
// interspersed. Common for tweets.
let tweetString = "Worst thing about working on String is that it breaks *everything*. Asserts, debuggers, and *especially* printf-style debugging 😭"

//
// Benchmarks
//

// Pre-commit benchmark: simple scalar walk
@inline(never)
public func run_StringWalk(_ N: Int) {
	return run_StringWalkASCIIScalars(N)
}

// Extended String benchmarks:
let baseMultiplier = 50_000
let scalarsMultiplier = baseMultiplier
let charactersMultiplier = baseMultiplier / 5

@inline(never)
public func run_StringWalkASCIIScalars(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars(asciiString.unicodeScalars)
  }
}

@inline(never)
public func run_StringWalkASCIICharacters(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters(asciiString.characters)
  }
}

@inline(never)
public func run_StringWalkUnicodeScalars(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars(utf16String.unicodeScalars)
  }
}

@inline(never)
public func run_StringWalkUnicodeCharacters(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters(utf16String.characters)
  }
}

@inline(never)
public func run_StringWalkMixedScalars(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars(tweetString.unicodeScalars)
  }
}

@inline(never)
public func run_StringWalkMixedCharacters(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters(tweetString.characters)
  }
}

@inline(never)
public func run_StringWalkASCIIScalarsBackwards(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars_rev(asciiString.unicodeScalars.reversed())
  }
}

@inline(never)
public func run_StringWalkASCIICharactersBackwards(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters_rev(asciiString.characters.reversed())
  }
}

@inline(never)
public func run_StringWalkUnicodeScalarsBackwards(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars_rev(utf16String.unicodeScalars.reversed())
  }
}

@inline(never)
public func run_StringWalkUnicodeCharactersBackwards(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters_rev(utf16String.characters.reversed())
  }
}

@inline(never)
public func run_StringWalkMixedScalarsBackwards(_ N: Int) {
  for _ in 1...scalarsMultiplier*N {
    countScalars_rev(tweetString.unicodeScalars.reversed())
  }
}

@inline(never)
public func run_StringWalkMixedCharactersBackwards(_ N: Int) {
  for _ in 1...charactersMultiplier*N {
    countCharacters_rev(tweetString.characters.reversed())
  }
}

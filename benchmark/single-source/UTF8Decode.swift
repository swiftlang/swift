//===--- UTF8Decode.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

@inline(never)
public func run_UTF8Decode(N: Int) {
  // 1-byte sequences
  // This test case is the longest as it's the most perfomance sensitive.
  let ascii = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."
  // 2-byte sequences
  let russian = "Ру́сский язы́к один из восточнославянских языков, национальный язык русского народа."
  // 3-byte sequences
  let japanese = "日本語（にほんご、にっぽんご）は、主に日本国内や日本人同士の間で使われている言語である。"
  // 4-byte sequences
  // Most commonly emoji, which are usually mixed with other text.
  let emoji = "Panda 🐼, Dog 🐶, Cat 🐱, Mouse 🐭."

  let strings = [ ascii, russian, japanese, emoji ].map { Array($0.utf8) }

  for _ in 1...200*N {
    for string in strings {
      var generator = string.generate()
      var utf8 = UTF8()
      while !utf8.decode(&generator).isEmptyInput() { }
    }
  }
}

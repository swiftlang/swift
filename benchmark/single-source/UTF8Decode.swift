//===--- UTF8Decode.swift -------------------------------------------------===//
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
import Foundation

public let UTF8Decode = [
    BenchmarkInfo(
      name: "UTF8Decode",
      runFunction: run_UTF8Decode,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitFromData",
      runFunction: run_UTF8Decode_InitFromData,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitDecoding",
      runFunction: run_UTF8Decode_InitDecoding,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitFromBytes",
      runFunction: run_UTF8Decode_InitFromBytes,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitFromData_ascii",
      runFunction: run_UTF8Decode_InitFromData_ascii,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitDecoding_ascii",
      runFunction: run_UTF8Decode_InitDecoding_ascii,
      tags: [.validation, .api, .String]),
    BenchmarkInfo(
      name: "UTF8Decode_InitFromBytes_ascii",
      runFunction: run_UTF8Decode_InitFromBytes_ascii,
      tags: [.validation, .api, .String]),
]

// 1-byte sequences
// This test case is the longest as it's the most performance sensitive.
let ascii = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."
let asciiBytes: [UInt8] = Array(ascii.utf8)
let asciiData: Data = Data(asciiBytes)

// 2-byte sequences
let russian = "Ру́сский язы́к один из восточнославянских языков, национальный язык русского народа."
// 3-byte sequences
let japanese = "日本語（にほんご、にっぽんご）は、主に日本国内や日本人同士の間で使われている言語である。"
// 4-byte sequences
// Most commonly emoji, which are usually mixed with other text.
let emoji = "Panda 🐼, Dog 🐶, Cat 🐱, Mouse 🐭."

let allStrings = [ascii, russian, japanese, emoji].map { Array($0.utf8) }
let allStringsBytes: [UInt8] = Array(allStrings.joined())
let allStringsData: Data = Data(allStringsBytes)


@inline(never)
public func run_UTF8Decode(_ N: Int) {
  let strings = allStrings

  func isEmpty(_ result: UnicodeDecodingResult) -> Bool {
    switch result {
    case .emptyInput:
      return true
    default:
      return false
    }
  }

  for _ in 1...200*N {
    for string in strings {
      var it = string.makeIterator()
      var utf8 = UTF8()
      while !isEmpty(utf8.decode(&it)) { }
    }
  }
}

@inline(never)
public func run_UTF8Decode_InitFromData(_ N: Int) {
  let input = allStringsData
  for _ in 0..<200*N {
    blackHole(String(data: input, encoding: .utf8))
  }
}
@inline(never)
public func run_UTF8Decode_InitDecoding(_ N: Int) {
  let input = allStringsBytes
  for _ in 0..<200*N {
    blackHole(String(decoding: input, as: UTF8.self))
  }
}
@inline(never)
public func run_UTF8Decode_InitFromBytes(_ N: Int) {
  let input = allStringsBytes
  for _ in 0..<200*N {
    blackHole(String(bytes: input, encoding: .utf8))
  }
}

@inline(never)
public func run_UTF8Decode_InitFromData_ascii(_ N: Int) {
  let input = asciiData
  for _ in 0..<1_000*N {
    blackHole(String(data: input, encoding: .utf8))
  }
}
@inline(never)
public func run_UTF8Decode_InitDecoding_ascii(_ N: Int) {
  let input = asciiBytes
  for _ in 0..<1_000*N {
    blackHole(String(decoding: input, as: UTF8.self))
  }
}
@inline(never)
public func run_UTF8Decode_InitFromBytes_ascii(_ N: Int) {
  let input = asciiBytes
  for _ in 0..<1_000*N {
    blackHole(String(bytes: input, encoding: .utf8))
  }
}



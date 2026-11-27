//===--- StringSmallStringOpt.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "StringCreateSmallASCII",
    runFunction: run_StringCreateSmallASCII,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringCreateSmallUTF8",
    runFunction: run_StringCreateSmallUTF8,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringFromBytesSmall1",
    runFunction: run_StringFromBytesSmall1,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringFromBytesSmall4",
    runFunction: run_StringFromBytesSmall4,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringFromBytesSmall8",
    runFunction: run_StringFromBytesSmall8,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringFromBytesSmall15",
    runFunction: run_StringFromBytesSmall15,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringSubstringSmall",
    runFunction: run_StringSubstringSmall,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "StringSubstringSpanWords",
    runFunction: run_StringSubstringSpanWords,
    tags: [.validation, .api, .String]),
]

// Test data for various small string sizes
let bytes1: [UInt8] = [72] // "H"
let bytes4: [UInt8] = [72, 101, 108, 108] // "Hell"
let bytes8: [UInt8] = [72, 101, 108, 108, 111, 32, 87, 111] // "Hello Wo"
let bytes15: [UInt8] = [72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 32, 33] // "Hello, World! !"

let asciiSmall = "Hello!"
let utf8Small = "Hëllo!"  
let mediumString = "Hello, World! This is a test."

@inline(never)
public func run_StringCreateSmallASCII(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes8, as: UTF8.self))
  }
}

@inline(never)
public func run_StringCreateSmallUTF8(_ N: Int) {
  let utf8Bytes: [UInt8] = [72, 195, 171, 108, 108, 111, 33] // "Hëllo!"
  for _ in 0..<N*10000 {
    blackHole(String(decoding: utf8Bytes, as: UTF8.self))
  }
}

@inline(never)
public func run_StringFromBytesSmall1(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes1, as: UTF8.self))
  }
}

@inline(never)
public func run_StringFromBytesSmall4(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes4, as: UTF8.self))
  }
}

@inline(never)
public func run_StringFromBytesSmall8(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes8, as: UTF8.self))
  }
}

@inline(never)
public func run_StringFromBytesSmall15(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes15, as: UTF8.self))
  }
}

@inline(never)
public func run_StringSubstringSmall(_ N: Int) {
  let str = "Hello, World!"
  for _ in 0..<N*10000 {
    blackHole(str[str.index(str.startIndex, offsetBy: 2)..<str.index(str.startIndex, offsetBy: 8)])
  }
}

@inline(never)
public func run_StringSubstringSpanWords(_ N: Int) {
  let str = "Hello, World! Test"
  for _ in 0..<N*5000 {
    // Extract substring that spans the SIMD4 boundary (8 bytes)
    blackHole(str[str.index(str.startIndex, offsetBy: 5)..<str.index(str.startIndex, offsetBy: 13)])
  }
}
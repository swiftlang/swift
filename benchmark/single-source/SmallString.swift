//===--- SmallString.swift ------------------------------------------------===//
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
    name: "SmallStringCreateASCII",
    runFunction: run_SmallStringCreateASCII,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringCreateUTF8",
    runFunction: run_SmallStringCreateUTF8,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringFromBytes1",
    runFunction: run_SmallStringFromBytes1,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringFromBytes4",
    runFunction: run_SmallStringFromBytes4,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringFromBytes8",
    runFunction: run_SmallStringFromBytes8,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringFromBytes14",
    runFunction: run_SmallStringFromBytes14,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringSubstring",
    runFunction: run_SmallStringSubstring,
    tags: [.validation, .api, .String]),
  BenchmarkInfo(
    name: "SmallStringSubstringSpanWords",
    runFunction: run_SmallStringSubstringSpanWords,
    tags: [.validation, .api, .String]),
]

let bytes1: [UInt8] = [72] // "H"
let bytes4: [UInt8] = [72, 101, 108, 108] // "Hell"
let bytes8: [UInt8] = [72, 101, 108, 108, 111, 32, 87, 111] // "Hello Wo"
let bytes14: [UInt8] = [72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 32] // "Hello, World! "

let asciiSmall = "Hello!"
let utf8Small = "Привіт!"  
let mediumString = "Hello, World! This is a test. 🚀"

@inline(never)
public func run_SmallStringCreateASCII(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes8, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringCreateUTF8(_ N: Int) {
  let utf8Bytes: [UInt8] = [208, 159, 209, 128, 208, 184, 208, 178, 209, 150, 209, 130, 33] // "Привіт!"
  for _ in 0..<N*10000 {
    blackHole(String(decoding: utf8Bytes, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringFromBytes1(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes1, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringFromBytes4(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes4, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringFromBytes8(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes8, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringFromBytes14(_ N: Int) {
  for _ in 0..<N*10000 {
    blackHole(String(decoding: bytes14, as: UTF8.self))
  }
}

@inline(never)
public func run_SmallStringSubstring(_ N: Int) {
  let str = "Hello, World!"
  for _ in 0..<N*10000 {
    blackHole(str[str.index(str.startIndex, offsetBy: 2)..<str.index(str.startIndex, offsetBy: 8)])
  }
}

// TODO: why is this needed
@inline(never)
public func run_SmallStringSubstringSpanWords(_ N: Int) {
  let str = "Hello, World! Test"
  for _ in 0..<N*5000 {
    // Extract substring that spans the SIMD4 boundary (8 bytes)
    blackHole(str[str.index(str.startIndex, offsetBy: 5)..<str.index(str.startIndex, offsetBy: 13)]) // ???
  }
}

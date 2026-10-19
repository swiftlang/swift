//===--- StringValidating.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// The benchmarks in UTF8Decode/UTF16Decode focus on the per-element cost with
// long buffers, which hides fixed costs. Since most Strings are short, we do
// actually care about those fixed costs, so this measures them.
import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "StringValidating.ascii.4",
    runFunction: { run_ascii(asciiInput4, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
  BenchmarkInfo(
    name: "StringValidating.ascii.64",
    runFunction: { run_ascii(asciiInput64, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
  BenchmarkInfo(
    name: "StringValidating.utf8.4",
    runFunction: { run_utf8(utf8Input4, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
  BenchmarkInfo(
    name: "StringValidating.utf8.64",
    runFunction: { run_utf8(utf8Input64, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
  BenchmarkInfo(
    name: "StringValidating.utf16.4",
    runFunction: { run_utf16(utf16Input4, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
  BenchmarkInfo(
    name: "StringValidating.utf16.64",
    runFunction: { run_utf16(utf16Input64, $0) },
    tags: [.validation, .api, .String],
    setUpFunction: setUp),
]

func makeBuffer<T>(_ elements: [T]) -> UnsafeBufferPointer<T> {
  let buffer = UnsafeMutableBufferPointer<T>.allocate(capacity: elements.count)
  _ = buffer.initialize(from: elements)
  return UnsafeBufferPointer(buffer)
}

func makeASCII(_ count: Int) -> UnsafeBufferPointer<UInt8> {
  makeBuffer((0..<count).map { UInt8(ascii: "a") + UInt8($0 % 26) })
}

// "é" repeated: two bytes per scalar.
func makeUTF8(_ count: Int) -> UnsafeBufferPointer<UInt8> {
  makeBuffer((0..<count).map { $0 % 2 == 0 ? 0xC3 : 0xA9 })
}

func makeUTF16(_ count: Int) -> UnsafeBufferPointer<UInt16> {
  makeBuffer((0..<count).map { 0x4E00 + UInt16($0 % 100) })
}

let asciiInput4 = makeASCII(4)
let asciiInput64 = makeASCII(64)
let utf8Input4 = makeUTF8(4)
let utf8Input64 = makeUTF8(64)
let utf16Input4 = makeUTF16(4)
let utf16Input64 = makeUTF16(64)

func setUp() {
  blackHole(asciiInput4)
  blackHole(asciiInput64)
  blackHole(utf8Input4)
  blackHole(utf8Input64)
  blackHole(utf16Input4)
  blackHole(utf16Input64)
}

@inline(never)
func run_ascii(_ input: UnsafeBufferPointer<UInt8>, _ n: Int) {
  guard #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) else { return }
  for _ in 0..<10_000*n {
    blackHole(String(validating: identity(input), as: Unicode.ASCII.self))
  }
}

@inline(never)
func run_utf8(_ input: UnsafeBufferPointer<UInt8>, _ n: Int) {
  guard #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) else { return }
  for _ in 0..<10_000*n {
    blackHole(String(validating: identity(input), as: UTF8.self))
  }
}

@inline(never)
func run_utf16(_ input: UnsafeBufferPointer<UInt16>, _ n: Int) {
  guard #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) else { return }
  for _ in 0..<10_000*n {
    blackHole(String(validating: identity(input), as: UTF16.self))
  }
}

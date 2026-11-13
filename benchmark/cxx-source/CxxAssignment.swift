//===--- CxxAssignment.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This is a benchmark that tracks how quickly Swift can assign a C++ container
// to another C++ container of the same size, either by copying an existing
// container or by moving a temporary one.

import TestsUtils
import CxxStdlibPerformance
import CxxStdlib // FIXME(rdar://128520766): this import should be redundant

public let benchmarks = [
  BenchmarkInfo(
    name: "CxxVecU32.assign.copy",
    runFunction: run_CxxVectorOfU32_assign_copy,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeVectorOnce),
  BenchmarkInfo(
    name: "CxxVecU32.assign.move",
    runFunction: run_CxxVectorOfU32_assign_move,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeVectorOnce),
  BenchmarkInfo(
    name: "CxxSetU32.assign.copy",
    runFunction: run_CxxSetOfU32_assign_copy,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeSetOnce),
  BenchmarkInfo(
    name: "CxxSetU32.assign.move",
    runFunction: run_CxxSetOfU32_assign_move,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeSetOnce),
]

func makeVectorOnce() {
  initVector(vectorSize)
}

func makeSetOnce() {
  initSet(setSize)
}

let vectorSize = 100
let setSize = 1_000
let iterRepeatFactor = 50

@inline(never)
public func run_CxxVectorOfU32_assign_copy(_ n: Int) {
  let source = makeVector32(vectorSize)
  var destination = source
  for _ in 0..<(n * iterRepeatFactor) {
    destination = source
    blackHole(destination)
  }
}

@inline(never)
func assignTemporary(to destination: inout VectorOfU32) {
  destination = makeVector32(vectorSize)
}

@inline(never)
public func run_CxxVectorOfU32_assign_move(_ n: Int) {
  var destination = makeVector32(vectorSize)
  for _ in 0..<(n * iterRepeatFactor) {
    assignTemporary(to: &destination)
  }
  blackHole(destination)
}

@inline(never)
public func run_CxxSetOfU32_assign_copy(_ n: Int) {
  let source = makeSet32(setSize)
  var destination = source
  for _ in 0..<(n * iterRepeatFactor) {
    destination = source
    blackHole(destination)
  }
}

@inline(never)
func assignTemporary(to destination: inout SetOfU32) {
  destination = makeSet32(setSize)
}

@inline(never)
public func run_CxxSetOfU32_assign_move(_ n: Int) {
  var destination = makeSet32(setSize)
  for _ in 0..<(n * iterRepeatFactor) {
    assignTemporary(to: &destination)
  }
  blackHole(destination)
}

//===--- ArrayOfPOD.swift -------------------------------------------------===//
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

// This benchmark tests creation and destruction of an array of
// trivial static type. It is meant to be a baseline for comparison against
// ArrayOfGenericPOD.
//
// For comparison, we always create three arrays of 200,000 words.

import TestsUtils

public let ArrayOfPOD = BenchmarkInfo(
  name: "ArrayOfPOD",
  runFunction: run_ArrayOfPOD,
  tags: [.validation, .api, .Array])

class RefArray<T> {
  var array : [T]

  init(_ i:T, count:Int = 100_000) {
    array = [T](repeating: i, count: count)
  }
}

@inline(never)
func genIntArray() {
  blackHole(RefArray<Int>(3, count:200_000))
  // should be a nop
}

enum PODEnum {
  case Some(Int)

  init(i:Int) { self = .Some(i) }
}

@inline(never)
func genEnumArray() {
  blackHole(RefArray<PODEnum>(PODEnum.Some(3)))
  // should be a nop
}

struct S {
  var x: Int
  var y: Int
}
@inline(never)
func genStructArray() {
  blackHole(RefArray<S>(S(x:3, y:4)))
  // should be a nop
}

@inline(never)
public func run_ArrayOfPOD(_ N: Int) {
  for _ in 0..<N {
    genIntArray()
    genEnumArray()
    genStructArray()
  }
}

//===--- ArrayOfGenericPOD.swift ------------------------------------------===//
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

// This benchmark tests creation and destruction of arrays of enum and
// generic type bound to trivial types. It should take the same time as
// ArrayOfPOD.  (In practice, it takes a little longer to construct
// the optional arrays).
//
// For comparison, we always create three arrays of 200,000 words.
// An integer enum takes two words.

import TestsUtils

public let ArrayOfGenericPOD = [
  BenchmarkInfo(
    // Renamed benchmark to "2" when IUO test was removed, which
    // effectively changed what we're benchmarking here.
    name: "ArrayOfGenericPOD2",
    runFunction: run_ArrayOfGenericPOD,
    tags: [.validation, .api, .Array]),

  // Initialize an array of generic POD from a slice.
  // This takes a unique path through stdlib customization points.
  BenchmarkInfo(
    name: "ArrayInitFromSlice",
    runFunction: run_initFromSlice,
    tags: [.validation, .api, .Array], setUpFunction: createArrayOfPOD)
]

class RefArray<T> {
  var array: [T]

  init(_ i:T) {
    array = [T](repeating: i, count: 100000)
  }
}

// Check the performance of destroying an array of enums (optional) where the
// enum has a single payload of trivial type. Destroying the
// elements should be a nop.
@inline(never)
func genEnumArray() {
  blackHole(RefArray<Int?>(3))
  // should be a nop
}

// Check the performance of destroying an array of structs where the
// struct has multiple fields of trivial type. Destroying the
// elements should be a nop.
struct S<T> {
  var x: T
  var y: T
}
@inline(never)
func genStructArray() {
  blackHole(RefArray<S<Int>>(S(x:3, y:4)))
  // should be a nop
}

@inline(never)
public func run_ArrayOfGenericPOD(_ N: Int) {
  for _ in 0..<N {
    genEnumArray()
    genStructArray()
  }
}

// --- ArrayInitFromSlice

let globalArray = Array<UInt8>(repeating: 0, count: 4096)

func createArrayOfPOD() {
  blackHole(globalArray)
}

@inline(never)
@_optimize(none)
func copyElements<S: Sequence>(_ contents: S) -> [UInt8]
  where S.Iterator.Element == UInt8
{
  return [UInt8](contents)
}

@inline(never)
public func run_initFromSlice(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<1000 {
      // Slice off at least one element so the array buffer can't be reused.
      blackHole(copyElements(globalArray[0..<4095]))
    }
  }
}

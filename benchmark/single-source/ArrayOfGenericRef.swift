//===--- ArrayOfGenericRef.swift ------------------------------------------===//
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

// This benchmark tests creation and destruction of an array of enum
// and generic type bound to nontrivial types.
//
// For comparison, we always create three arrays of 1,000 words.

import TestsUtils

public let ArrayOfGenericRef = BenchmarkInfo(
  name: "ArrayOfGenericRef",
  runFunction: run_ArrayOfGenericRef,
  tags: [.validation, .api, .Array],
  legacyFactor: 10)

protocol Constructible {
  associatedtype Element
  init(e:Element)
}
class ConstructibleArray<T:Constructible> {
  var array: [T]

  init(_ e:T.Element) {
    array = [T]()
    array.reserveCapacity(1_000)
    for _ in 0...1_000 {
      array.append(T(e:e))
    }
  }
}

class GenericRef<T> : Constructible {
  typealias Element=T
  var x: T
  required init(e:T) { self.x = e }
}

// Reference to a POD class.
@inline(never)
func genPODRefArray() {
  blackHole(ConstructibleArray<GenericRef<Int>>(3))
  // should be a nop
}

class Dummy {}

// Reference to a reference. The nested reference is shared across elements.
@inline(never)
func genCommonRefArray() {
  let d = Dummy()
  blackHole(ConstructibleArray<GenericRef<Dummy>>(d))
  // should be a nop
}

// Reuse the same enum value for each element.
class RefArray<T> {
  var array: [T]

  init(_ i:T, count:Int = 1_000) {
    array = [T](repeating: i, count: count)
  }
}

// enum holding a reference.
@inline(never)
func genRefEnumArray() {
  let d = Dummy()
  blackHole(RefArray<Dummy?>(d))
  // should be a nop
}

struct GenericVal<T> : Constructible {
  typealias Element=T
  var x: T
  init(e:T) { self.x = e }
}

// Struct holding a reference.
@inline(never)
func genRefStructArray() {
  let d = Dummy()
  blackHole(ConstructibleArray<GenericVal<Dummy>>(d))
  // should be a nop
}

@inline(never)
public func run_ArrayOfGenericRef(_ N: Int) {
  for _ in 0..<N {
    genPODRefArray()
    genCommonRefArray()
    genRefEnumArray()
    genRefStructArray()
  }
}

//===--- ArrayOfRef.swift -------------------------------------------------===//
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
// references. It is meant to be a baseline for comparison against
// ArrayOfGenericRef.
//
// For comparison, we always create four arrays of 10,000 words.

import TestsUtils

public let ArrayOfRef = BenchmarkInfo(
  name: "ArrayOfRef",
  runFunction: run_ArrayOfRef,
  tags: [.validation, .api, .Array])

protocol Constructible {
  associatedtype Element
  init(e:Element)
}
class ConstructibleArray<T:Constructible> {
  var array : [T]

  init(_ e:T.Element) {
    array = [T]()
    array.reserveCapacity(10_000)
    for _ in 0...10_000 {
      array.append(T(e:e) as T)
    }
  }
}

// Reference to a POD class.
class POD : Constructible {
  typealias Element=Int
  var x: Int
  required init(e:Int) { self.x = e }
}

@inline(never)
func genPODRefArray() {
  blackHole(ConstructibleArray<POD>(3))
  // should be a nop
}

class Dummy {}

// Reference to a reference. The nested reference is shared across elements.
class CommonRef : Constructible {
  typealias Element=Dummy
  var d: Dummy
  required init(e:Dummy) { self.d = e }
}

@inline(never)
func genCommonRefArray() {
  let d = Dummy()
  blackHole(ConstructibleArray<CommonRef>(d))
  // should be a nop
}

enum RefEnum {
  case None
  case Some(Dummy)
}

// Reuse the same enum value for each element.
class RefArray<T> {
  var array : [T]

  init(_ i:T, count:Int = 10_000) {
    array = [T](repeating: i, count: count)
  }
}

@inline(never)
func genRefEnumArray() {
  let e = RefEnum.Some(Dummy())
  blackHole(RefArray<RefEnum>(e))
  // should be a nop
}

// Struct holding a reference.
struct S : Constructible {
  typealias Element=Dummy
  var d: Dummy
  init(e:Dummy) { self.d = e }
}

@inline(never)
func genRefStructArray() {
  let d = Dummy()
  blackHole(ConstructibleArray<S>(d))
  // should be a nop
}

@inline(never)
public func run_ArrayOfRef(_ N: Int) {
  for _ in 0..<N {
    genPODRefArray()
    genCommonRefArray()
    genRefEnumArray()
    genRefStructArray()
  }
}

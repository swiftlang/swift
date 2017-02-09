//===--- StaticArray.swift ------------------------------------------------===//
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
//
// We use this test to benchmark the compile time and analyze the code
// generation of struct initializers.
//===----------------------------------------------------------------------===//

import TestsUtils

protocol StaticArrayProtocol {
  associatedtype ElemTy
  init(_ defaultValue : ElemTy)
  func get(_ idx : Int) -> ElemTy
  mutating func set(_ idx : Int,_ val : ElemTy)
  func count() -> Int
}

struct A0<ElemTy> : StaticArrayProtocol {
  init(_ defaultValue : ElemTy) { x = defaultValue }
  var x : ElemTy
  func get(_ idx : Int) -> ElemTy { if idx == 0 { return x } else { fatalError("oob"); } }
  mutating func set(_ idx : Int,_ val : ElemTy) { if idx == 0 { x = val }}
  func count() -> Int { return 1}
}

struct A2X<T : StaticArrayProtocol> : StaticArrayProtocol {
  init(_ defaultValue : T.ElemTy) { lower = T(defaultValue); upper = T(defaultValue) }
  var lower : T
  var upper : T
  func get(_ idx: Int) -> T.ElemTy { let size = lower.count(); if idx < size { return lower.get(idx) } else { return upper.get(idx - size) }}
  mutating func set(_ idx: Int,_ val : T.ElemTy) {let size = lower.count(); if idx < size { return lower.set(idx, val) } else { return upper.set(idx - size, val) }}
  func count() -> Int { return upper.count() + lower.count() }
}

struct StaticArray<
  T : StaticArrayProtocol
> : StaticArrayProtocol, RandomAccessCollection, MutableCollection {
  typealias Indices = CountableRange<Int>
  
  init(_ defaultValue : T.ElemTy) { values = T(defaultValue) }
  var values : T
  func get(_ idx: Int) -> T.ElemTy { return values.get(idx) }
  mutating func set(_ idx: Int,_ val : T.ElemTy) { return values.set(idx, val) }
  func count() -> Int { return values.count() }

  typealias Index = Int
  typealias IndexDistance = Int
  let startIndex: Int = 0
  var endIndex: Int { return count()}

  subscript(idx: Int) -> T.ElemTy {
    get {
      return get(idx)
    }
    set(val) {
      set(idx, val)
    }
  }

  typealias Iterator = IndexingIterator<StaticArray>

  subscript(bounds: Range<Index>) -> StaticArray<T> {
    get { fatalError() }
    set { fatalError() }
  }
}

typealias SA2Int   = StaticArray<A0<Int>>
typealias SA4Int   = StaticArray<A2X<A0<Int>>>
typealias SA8Int   = StaticArray<A2X<A2X<A0<Int>>>>
typealias SA16Int  = StaticArray<A2X<A2X<A2X<A0<Int>>>>>
typealias SA32Int  = StaticArray<A2X<A2X<A2X<A2X<A0<Int>>>>>>
typealias SA64Int  = StaticArray<A2X<A2X<A2X<A2X<A2X<A0<Int>>>>>>>
typealias SA128Int = StaticArray<A2X<A2X<A2X<A2X<A2X<A2X<A0<Int>>>>>>>>

// Make sure the optimizer does not optimize the compute away.
@inline(never)
public func sink(_ value: Int) { if False() { print(value) }}


@inline(never)
public func run_StaticArray(_ N: Int) {

  for _ in 1...N {
    var staticArray = SA128Int(0)
    for i in 0..<staticArray.count() { staticArray[i] = i ^ 123 }
    staticArray.sort()
    sink(staticArray[0])
  }
}

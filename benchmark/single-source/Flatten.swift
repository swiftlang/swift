//===--- Flatten.swift -------------------------------------  -*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to Flatten.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

/// The `Flatten` benchmark family tests the performance of `flatMap` function
/// and functionally equivalent `map.joined`, along with their lazy variants
/// relative to an imperative approach with simple for-in loop across a
/// selection of representative types.
///
/// For transforming fully materialized collection with contiguous memory layout
/// additional Unsafe versions were created as attempts at manual optimization
/// that try to eliminate the abstraction overhead using assumptions about the
/// internal memory layout of underlying data structures.
///
/// # Colors
///
/// First hypotetical scenario is transforming an array of RBGA pixel values
/// represented as `struct ColorVal { let r, g, b, a: UInt8 }` to flat [UInt8]
/// in ARGB format. In case of [ColorVal], this means that the real work being
/// performed is copying of byte swizzled raw memory, obfuscated by type casting
/// and higher-level abstractions (structs, arrays).
///
/// The alternative type `class ColorRef` demonstrates the impact of using
/// reference type.
///
/// After experimenting with Unsafe variants, conforming the `ColorVal` to
/// `Sequence` protocol by creating a custom iterator, which performes the color
/// component swizzling, showed promising performance (better than imperative
/// approach). That varaint is called `SwizSeq`. Turns out that conforming
/// the type to `Collection` protocol (in `SwizCol` variant) is even better,
/// allowing compiler to optimize the lazy variants for almost 4x gain, beating
/// even the best performing Unsafe variant that copies the colors byte-by-byte.
///
/// See http://wiki.c2.com/?SufficientlySmartCompiler
///
/// The Unsafe variants were originally meant as aspirational goals for the
/// functional approach, but are now kept here as artifacts of partial
/// improvements over the imperative approach, which demonstrate unexpected
/// performance behavior.
///
/// # Tuple4 and Array4
///
/// Second scenario tests the performance of flattening the compound type
/// `(Int, Int, Int, Int)`, typealiased as Tuple4, into [Int]. This variant
/// compensates for the larger data type by ommiting the structural
/// transformation. In case of fully materialized collection, [Tuple4], the real
/// work is simply a type cast. There's currently no Array API to perform this
/// in O(1), pending [SE-0223](https://forums.swift.org/t/15194/41). Therefore
/// the Unsafe variants perform a simple memory copy.
///
/// Next variant, `Array4` uses 4 element "static" array instead of the Tuple4,
/// and is meant do demonstrate the relative cost of switching to this currency
/// type. This is important, because Array is naturally used, thanks to its
/// syntactic sugar, as the flattened collection in all the functional-style
/// scenarios, i.e. the closures in `flatMap` and `map.joined` are producing
/// "static" arrays on the fly.
///
/// The `Tuple4` and `Array4` type groups are varied across 3 container types:
///
/// * `Flatten.Array` is a fully materialized collection,
/// * `Flatten.LazySeq` is lazily generated sequence, and
/// * `Flatten.AnySeq.LazySeq` is a type erased version of the latter.
///
/// After [SE-0234](https://forums.swift.org/t/18002), no standard library API
/// returns `AnySequence` anymore, but the tests
/// `Flatten.LazySeq.Tuple4.flatMap` and `Flatten.AnySeq.LazySeq.Tuple4.flatMap`
/// hint at the hidden potential for improvement in the utter performance
/// debacle that is `Flatten.LazySeq`. The `AnySeq` group could be removed in
/// the future, when _that_ deoptimization is properly addressed.

let t: [BenchmarkCategory] = [.api, .validation]

public let Flatten = [
  BenchmarkInfo(name: "Flatten.Array.Tuple4.flatMap",
    runFunction: run_FlattenArrayTuple4_flatMap, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.map.joined",
    runFunction: run_FlattenArrayTuple4_map_joined, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.for-in",
    runFunction: run_FlattenArrayTuple4_forin, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.for-in.Reserve",
    runFunction: run_FlattenArrayTuple4_forinReserve, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.Unsafe.InitSeq",
    runFunction: run_FlattenArrayTuple4UnsafeInitSeq, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.Unsafe.IntsReserve",
    runFunction: run_FlattenArrayTuple4UnsafeIntsReserve, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.lazy.flatMap",
    runFunction: run_FlattenArrayTuple4_lazy_flatMap, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.lazy.map.joined",
    runFunction: run_FlattenArrayTuple4_lazy_map_joined, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.Array.Tuple4.lazy.for-in",
    runFunction: run_FlattenArrayTuple4_lazy_forin, tags: t,
		setUpFunction: sat4),
  BenchmarkInfo(name: "Flatten.LazySeq.Tuple4.flatMap",
    runFunction: run_FlattenLazySeqTuple4_flatMap, tags: t),
  BenchmarkInfo(name: "Flatten.LazySeq.Tuple4.map.joined",
    runFunction: run_FlattenLazySeqTuple4_map_joined, tags: t),
  BenchmarkInfo(name: "Flatten.LazySeq.Tuple4.for-in",
    runFunction: run_FlattenLazySeqTuple4_forin, tags: t),
  BenchmarkInfo(name: "Flatten.AnySeq.LazySeq.Tuple4.flatMap",
    runFunction: run_FlattenAnySeqLazySeqTuple4_flatMap, tags: t),
  BenchmarkInfo(name: "Flatten.AnySeq.LazySeq.Tuple4.map.joined",
    runFunction: run_FlattenAnySeqLazySeqTuple4_map_joined, tags: t),
  BenchmarkInfo(name: "Flatten.AnySeq.LazySeq.Tuple4.for-in",
    runFunction: run_FlattenAnySeqLazySeqTuple4_forin, tags: t),
  BenchmarkInfo(name: "Flatten.Array.Array4.flatMap",
    runFunction: run_FlattenArrayArray4_flatMap, tags: t,
		setUpFunction: saa4),
  BenchmarkInfo(name: "Flatten.Array.Array4.joined",
    runFunction: run_FlattenArrayArray4_joined, tags: t,
		setUpFunction: saa4),
  BenchmarkInfo(name: "Flatten.Array.Array4.for-in",
    runFunction: run_FlattenArrayArray4_forin, tags: t,
		setUpFunction: saa4),
  BenchmarkInfo(name: "Flatten.Array.Array4.lazy.flatMap",
    runFunction: run_FlattenArrayArray4_lazy_flatMap, tags: t,
		setUpFunction: saa4),
  BenchmarkInfo(name: "Flatten.Array.Array4.lazy.joined",
    runFunction: run_FlattenArrayArray4_lazy_joined, tags: t,
		setUpFunction: saa4),
  BenchmarkInfo(name: "Flatten.LazySeq.Array4.flatMap",
    runFunction: run_FlattenLazySeqArray4_flatMap, tags: t),
  BenchmarkInfo(name: "Flatten.LazySeq.Array4.joined",
    runFunction: run_FlattenLazySeqArray4_joined, tags: t),
  BenchmarkInfo(name: "Flatten.LazySeq.Array4.for-in",
    runFunction: run_FlattenLazySeqArray4_forin, tags: t),
  BenchmarkInfo(name: "Flatten.ColorVal.flatMap.Array",
    runFunction: run_FlattenColorVal_flatMapArray, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.flatMap.ContArr",
    runFunction: run_FlattenColorVal_flatMapContArr, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.flatMap.SwizCol",
    runFunction: run_FlattenColorVal_flatMapSwizCol, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.flatMap.SwizSeq",
    runFunction: run_FlattenColorVal_flatMapSwizSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.map.joined.Array",
    runFunction: run_FlattenColorVal_map_joinedArray, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.map.joined.ContArr",
    runFunction: run_FlattenColorVal_map_joinedContArr, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.map.joined.SwizCol",
    runFunction: run_FlattenColorVal_map_joinedSwizCol, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.map.joined.SwizSeq",
    runFunction: run_FlattenColorVal_map_joinedSwizSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.flatMap.Array",
    runFunction: run_FlattenColorVal_lazy_flatMapArray, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.flatMap.ContArr",
    runFunction: run_FlattenColorVal_lazy_flatMapContArr, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.flatMap.SwizCol",
    runFunction: run_FlattenColorVal_lazy_flatMapSwizCol, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.flatMap.SwizSeq",
    runFunction: run_FlattenColorVal_lazy_flatMapSwizSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.map.joined.Array",
    runFunction: run_FlattenColorVal_lazy_map_joinedArray, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.map.joined.ContArr",
    runFunction: run_FlattenColorVal_lazy_map_joinedContArr, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.map.joined.SwizCol",
    runFunction: run_FlattenColorVal_lazy_map_joinedSwizCol, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.lazy.map.joined.SwizSeq",
    runFunction: run_FlattenColorVal_lazy_map_joinedSwizSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.for-in",
    runFunction: run_FlattenColorVal_forin, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.for-in.Reserve",
    runFunction: run_FlattenColorVal_forinReserve, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.FlatMapArray",
    runFunction: run_FlattenColorValUnsafeFlatMapArray, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.UInt32InitSeq",
    runFunction: run_FlattenColorValUnsafeUInt32InitSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.ColorValInitSeq",
    runFunction: run_FlattenColorValUnsafeColorValInitSeq, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.FlatMapColorVal",
    runFunction: run_FlattenColorValUnsafeFlatMapColorVal, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.BytesReserve",
    runFunction: run_FlattenColorValUnsafeBytesReserve, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorVal.Unsafe.Bytes",
    runFunction: run_FlattenColorValUnsafeBytes, tags: t,
		setUpFunction: sacr),
  BenchmarkInfo(name: "Flatten.ColorRef.flatMap.Array",
    runFunction: run_FlattenColorRef_flatMapArray, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.flatMap.ContArr",
    runFunction: run_FlattenColorRef_flatMapContArr, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.flatMap.SwizCol",
    runFunction: run_FlattenColorRef_flatMapSwizCol, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.flatMap.SwizSeq",
    runFunction: run_FlattenColorRef_flatMapSwizSeq, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.map.joined.Array",
    runFunction: run_FlattenColorRef_map_joinedArray, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.map.joined.ContArr",
    runFunction: run_FlattenColorRef_map_joinedContArr, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.map.joined.SwizCol",
    runFunction: run_FlattenColorRef_map_joinedSwizCol, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.map.joined.SwizSeq",
    runFunction: run_FlattenColorRef_map_joinedSwizSeq, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.flatMap.Array",
    runFunction: run_FlattenColorRef_lazy_flatMapArray, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.flatMap.ContArr",
    runFunction: run_FlattenColorRef_lazy_flatMapContArr, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.flatMap.SwizCol",
    runFunction: run_FlattenColorRef_lazy_flatMapSwizCol, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.flatMap.SwizSeq",
    runFunction: run_FlattenColorRef_lazy_flatMapSwizSeq, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.map.joined.Array",
    runFunction: run_FlattenColorRef_lazy_map_joinedArray, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.map.joined.ContArr",
    runFunction: run_FlattenColorRef_lazy_map_joinedContArr, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.map.joined.SwizCol",
    runFunction: run_FlattenColorRef_lazy_map_joinedSwizCol, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.lazy.map.joined.SwizSeq",
    runFunction: run_FlattenColorRef_lazy_map_joinedSwizSeq, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.for-in",
    runFunction: run_FlattenColorRef_forin, tags: t,
		setUpFunction: sacv),
  BenchmarkInfo(name: "Flatten.ColorRef.for-in.Reserve",
    runFunction: run_FlattenColorRef_forinReserve, tags: t,
		setUpFunction: sacv),
]

// Setup functions
func sat4() { blackHole(arrayTuple4) }
func saa4() { blackHole(arrayArray4) }
func sacv() { blackHole(arrayColorVals) }
func sacr() { blackHole(arrayColorRefs) }

// Workloads and test constants
let size = 1<<14
let flatSize = size * 4
let lastElement = size + 3

typealias Tuple4 = (Int, Int, Int, Int)
func tuple4(_ i: Int) -> Tuple4 { return (i, i + 1, i + 2, i + 3) }
let arrayTuple4 = (1...size).map(tuple4)
let sequenceTuple4 = (1...size).lazy.map(tuple4)

func array4(_ i: Int) -> [Int] { return [i, i + 1, i + 2, i + 3] }
let arrayArray4 = (1...size).map(array4)
let sequenceArray4 = (1...size).lazy.map(array4)

func c(_ i: Int) -> UInt8 { return UInt8(i % 256) } // clamp
let arrayColorVals = (1...size).map {
  ColorVal(r: c($0), g: c($0 + 1), b: c($0 + 2), a: c($0 + 3))
}
let arrayColorRefs = (1...size).map {
  ColorRef(r: c($0), g: c($0 + 1), b: c($0 + 2), a: c($0 + 3))
}
let lastBlue = c(size + 2)

// 32-bit color
struct ColorVal { let r, g, b, a: UInt8 }
final class ColorRef {
  let r, g, b, a: UInt8
  init(r: UInt8, g: UInt8, b: UInt8, a: UInt8) {
    self.r = r; self.g = g; self.b = b; self.a = a
  }
}

// Helper for materializing Sequence.
// It counts the length and retains the last element.
struct CountLast<T> {
  var count: Int = 0
  var last: T? = nil
  static func collect(_ cl: inout CountLast, _ element: T){
    cl.count += 1
    cl.last = element
  }
}

// Byte swizzling Sequence and Collection conformances for Color types
extension ColorVal {
  public struct SwizzleSequence {
    let c: ColorVal
    var i: UInt8 = 0
    init(_ c: ColorVal) { self.c = c }
  }

  public struct SwizzleCollection {
    let c: ColorVal
    init(_ c: ColorVal) { self.c = c }
  }
}

extension ColorVal.SwizzleSequence: Sequence, IteratorProtocol {
  public var underestimatedCount: Int { return 4 }
  public mutating func next() -> UInt8? {
    i = i &+ 1
    switch i {
    case 1: return c.a
    case 2: return c.r
    case 3: return c.g
    case 4: return c.b
    default: return nil
    }
  }
}

extension ColorVal.SwizzleCollection: RandomAccessCollection {
  public var count: Int { return 4 }
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return 4 }
  public func index(after i: Int) -> Int { return i &+ 1 }
  public subscript(position: Int) -> UInt8 {
    switch position {
    case 0: return c.a
    case 1: return c.r
    case 2: return c.g
    case 3: return c.b
    default: return 0 // never happens
    }
  }
}

extension ColorVal {
  public var sequence: SwizzleSequence { return SwizzleSequence(self) }
  public var collection: SwizzleCollection { return SwizzleCollection(self) }
}

extension ColorRef {
  public struct SwizzleSequence {
    let c: ColorRef
    var i: UInt8 = 0
    init(_ c: ColorRef) { self.c = c }
  }

  public struct SwizzleCollection {
    let c: ColorRef
    init(_ c: ColorRef) { self.c = c }
  }
}

extension ColorRef.SwizzleSequence: Sequence, IteratorProtocol {
  public var underestimatedCount: Int { return 4 }
  public mutating func next() -> UInt8? {
    i = i &+ 1
    switch i {
    case 1: return c.a
    case 2: return c.r
    case 3: return c.g
    case 4: return c.b
    default: return nil
    }
  }
}

extension ColorRef.SwizzleCollection: RandomAccessCollection {
  public var count: Int { return 4 }
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return 4 }
  public func index(after i: Int) -> Int { return i &+ 1 }
  public subscript(position: Int) -> UInt8 {
    switch position {
    case 0: return c.a
    case 1: return c.r
    case 2: return c.g
    case 3: return c.b
    default: return 0 // never happens
    }
  }
}

extension ColorRef {
  public var sequence: SwizzleSequence { return SwizzleSequence(self) }
  public var collection: SwizzleCollection { return SwizzleCollection(self) }
}


// Run functions
@inline(never)
public func run_FlattenArrayTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_forin(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    var f: [Int] = []

    for (x, y, z, w) in input {
      f.append(x)
      f.append(y)
      f.append(z)
      f.append(w)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_forinReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    var f: [Int] = []
    f.reserveCapacity(input.count * 4)

    for (x, y, z, w) in input {
      f.append(x)
      f.append(y)
      f.append(z)
      f.append(w)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4UnsafeInitSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    // Create an UnsafeBufferPointer that conforms to a Sequence viewing the
    // memory as a flattened sequence of 4 * count Ints. Construct a new Array
    // from this sequence using Array.init<Sequence>.
    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(
        to: Int.self, capacity: input.count * 4) {
        Array(UnsafeBufferPointer(start: $0, count: input.count * 4))
      }
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4UnsafeIntsReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    // Copy Int-by-Int.
    var f: [Int] = []
    let size = input.count * 4
    f.reserveCapacity(size)
    input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(to: Int.self, capacity: size) {
        ints in
        for i in 0..<size {
          f.append(ints[i])
        }
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_lazy_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4.lazy

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_lazy_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4.lazy

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayTuple4_lazy_forin(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4.lazy

    var f: [Int] = []

    for (x, y, z, w) in input {
      f.append(x)
      f.append(y)
      f.append(z)
      f.append(w)
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = sequenceTuple4

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = sequenceTuple4

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqTuple4_forin(_ N: Int) {
  for _ in 1...N {
    let input = sequenceTuple4

    var f: [Int] = []

    for (x, y, z, w) in input {
      f.append(x)
      f.append(y)
      f.append(z)
      f.append(w)
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqLazySeqTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceTuple4)

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqLazySeqTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceTuple4)

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqLazySeqTuple4_forin(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceTuple4)

    var f: [Int] = []

    for (x, y, z, w) in input {
      f.append(x)
      f.append(y)
      f.append(z)
      f.append(w)
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayArray4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4

    let f = input.flatMap { $0 }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayArray4_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4

    let f = input.joined()

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayArray4_forin(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4

    var f: [Int] = []

    for array in input {
      for element in array {
        f.append(element)
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayArray4_lazy_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4.lazy

    let f = input.flatMap { $0 }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayArray4_lazy_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4.lazy

    let f = input.joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqArray4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    let f = input.flatMap { $0 }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqArray4_joined(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    let f = input.joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenLazySeqArray4_forin(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    var f: [Int] = []

    for array in input {
      for element in array {
        f.append(element)
      }
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenColorVal_flatMapArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_flatMapContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.flatMap { ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_flatMapSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.flatMap { $0.collection }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_flatMapSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.flatMap { $0.sequence }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_map_joinedArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_map_joinedContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map({ ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_map_joinedSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map({ $0.collection }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_map_joinedSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map({ $0.sequence }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_flatMapArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_flatMapContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.flatMap { ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_flatMapSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.flatMap { $0.collection }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_flatMapSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.flatMap { $0.sequence }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_map_joinedArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_map_joinedContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.map({ ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_map_joinedSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.map({ $0.collection }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_lazy_map_joinedSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.map({ $0.sequence }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_forin(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    var f: [UInt8] = []

    for c in input {
      f.append(c.a)
      f.append(c.r)
      f.append(c.g)
      f.append(c.b)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorVal_forinReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    var f: [UInt8] = []
    f.reserveCapacity(input.count * 4)

    for c in input {
      f.append(c.a)
      f.append(c.r)
      f.append(c.g)
      f.append(c.b)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeFlatMapArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    // Swizzle the bytes in UInt32 view, then flatMap to [UInt8] using Array.
    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(
        to: UInt32.self, capacity: input.count) {
        UnsafeBufferPointer(start:$0, count: input.count).flatMap {
          (rawColor: UInt32) -> [UInt8] in
          let rgba = rawColor.bigEndian
          let argb = (rgba &<< 24) | (rgba &>> 8) // rotate
          return [
            UInt8(truncatingIfNeeded: argb &>> 24),
            UInt8(truncatingIfNeeded: argb &>> 16),
            UInt8(truncatingIfNeeded: argb &>> 8),
            UInt8(truncatingIfNeeded: argb)]
        }
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeUInt32InitSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    // Swizzle the bytes in UInt32 view, creating Array<UInt32>, then create
    // Array<UInt8> using Sequence initializer
    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(
        to: UInt32.self, capacity: input.count) {
        UnsafeBufferPointer(start:$0, count: input.count).map { rgba in
          // rotate (inverted direction, because x86 is little endian)
          (rgba &>> 24) | (rgba &<< 8)
        }
        .withUnsafeBytes(Array.init)
      }
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeColorValInitSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    // Swizzle components within the ColorVal type, then create Array<UInt8>
    // using Sequence initializer.
    // Similar to the non-swizzling `Flatten.Array.Tuple4.Unsafe.InitSeq`.
    let f = input.map { c in ColorVal(r: c.a, g: c.r, b: c.g, a: c.b) }
        .withUnsafeBytes(Array.init)

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeFlatMapColorVal(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.withUnsafeBufferPointer {
      $0.flatMap {
        (c: ColorVal) -> [UInt8] in
        [c.a, c.r, c.g, c.b]
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeBytesReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    // Copy the color components byte-by-byte with reserved capacity.
    var f = Array<UInt8>()
    let size = input.count * 4
    f.reserveCapacity(size)
    input.withUnsafeBytes { bytes in
      for i in stride(from: 0, to: size, by: 4) {
        f.append(bytes[i+3])
        f.append(bytes[i])
        f.append(bytes[i+1])
        f.append(bytes[i+2])
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorValUnsafeBytes(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    // Copy the color components byte-by-byte without reserved capacity.
    var f = Array<UInt8>()
    let size = input.count * 4
    input.withUnsafeBytes { bytes in
      for i in stride(from: 0, to: size, by: 4) {
        f.append(bytes[i+3])
        f.append(bytes[i])
        f.append(bytes[i+1])
        f.append(bytes[i+2])
      }
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_flatMapArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_flatMapContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.flatMap { ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_flatMapSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.flatMap { $0.collection }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_flatMapSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.flatMap { $0.sequence }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_map_joinedArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_map_joinedContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.map({ ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_map_joinedSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.map({ $0.collection }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_map_joinedSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.map({ $0.sequence }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_flatMapArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_flatMapContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.flatMap { ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_flatMapSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.flatMap { $0.collection }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_flatMapSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.flatMap { $0.sequence }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_map_joinedArray(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_map_joinedContArr(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.map({ ContiguousArray([$0.a, $0.r, $0.g, $0.b]) }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_map_joinedSwizCol(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.map({ $0.collection }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_lazy_map_joinedSwizSeq(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.map({ $0.sequence }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_forin(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    var f: [UInt8] = []

    for c in input {
      f.append(c.a)
      f.append(c.r)
      f.append(c.g)
      f.append(c.b)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenColorRef_forinReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    var f: [UInt8] = []
    f.reserveCapacity(input.count * 4)

    for c in input {
      f.append(c.a)
      f.append(c.r)
      f.append(c.g)
      f.append(c.b)
    }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}


// Local Variables:
// eval: (read-only-mode 1)
// End:

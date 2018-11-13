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

let t: [BenchmarkCategory] = [.api, .validation]

public let Flatten = [
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.flatMap",
    runFunction: run_FlattenArrayTuple4_flatMap, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.map.joined",
    runFunction: run_FlattenArrayTuple4_map_joined, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.for-in.Naive",
    runFunction: run_FlattenArrayTuple4_forinNaive, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.for-in.Reserve",
    runFunction: run_FlattenArrayTuple4_forinReserve, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.Unsafe",
    runFunction: run_FlattenArrayTuple4Unsafe, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.lazy.flatMap",
    runFunction: run_FlattenArrayTuple4_lazy_flatMap, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.lazy.map.joined",
    runFunction: run_FlattenArrayTuple4_lazy_map_joined, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.lazy.for-in.Naive",
    runFunction: run_FlattenArrayTuple4_lazy_forinNaive, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Tuple4.lazy.for-in.Reserve",
    runFunction: run_FlattenArrayTuple4_lazy_forinReserve, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.Seq.Tuple4.flatMap",
    runFunction: run_FlattenSeqTuple4_flatMap, tags: t),
  BenchmarkInfo(
    name: "Flatten.Seq.Tuple4.map.joined",
    runFunction: run_FlattenSeqTuple4_map_joined, tags: t),
  BenchmarkInfo(
    name: "Flatten.Seq.Tuple4.for-in.Naive",
    runFunction: run_FlattenSeqTuple4_forinNaive, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Tuple4.flatMap",
    runFunction: run_FlattenAnySeqArrayTuple4_flatMap, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Tuple4.map.joined",
    runFunction: run_FlattenAnySeqArrayTuple4_map_joined, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Tuple4.for-in.Naive",
    runFunction: run_FlattenAnySeqArrayTuple4_forinNaive, tags: t,
		setUpFunction: { blackHole(arrayTuple4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Tuple4.flatMap",
    runFunction: run_FlattenAnySeqSeqTuple4_flatMap, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Tuple4.map.joined",
    runFunction: run_FlattenAnySeqSeqTuple4_map_joined, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Tuple4.for-in.Naive",
    runFunction: run_FlattenAnySeqSeqTuple4_forinNaive, tags: t),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.flatMap",
    runFunction: run_FlattenArrayArray4_flatMap, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.joined",
    runFunction: run_FlattenArrayArray4_joined, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.for-in.Elem",
    runFunction: run_FlattenArrayArray4_forinElem, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.for-in.Batch",
    runFunction: run_FlattenArrayArray4_forinBatch, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.lazy.flatMap",
    runFunction: run_FlattenArrayArray4_lazy_flatMap, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.lazy.joined",
    runFunction: run_FlattenArrayArray4_lazy_joined, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.lazy.for-in.Elem",
    runFunction: run_FlattenArrayArray4_lazy_forinElem, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Array.Array4.lazy.for-in.Batch",
    runFunction: run_FlattenArrayArray4_lazy_forinBatch, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.Seq.Array4.flatMap",
    runFunction: run_FlattenSeqArray4_flatMap, tags: t),
  BenchmarkInfo(
    name: "Flatten.Seq.Array4.joined",
    runFunction: run_FlattenSeqArray4_joined, tags: t),
  BenchmarkInfo(
    name: "Flatten.Seq.Array4.for-in.Elem",
    runFunction: run_FlattenSeqArray4_forinElem, tags: t),
  BenchmarkInfo(
    name: "Flatten.Seq.Array4.for-in.Batch",
    runFunction: run_FlattenSeqArray4_forinBatch, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Array4.flatMap",
    runFunction: run_FlattenAnySeqArrayArray4_flatMap, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Array4.joined",
    runFunction: run_FlattenAnySeqArrayArray4_joined, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Array4.for-in.Elem",
    runFunction: run_FlattenAnySeqArrayArray4_forinElem, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Array.Array4.for-in.Batch",
    runFunction: run_FlattenAnySeqArrayArray4_forinBatch, tags: t,
		setUpFunction: { blackHole(arrayArray4) }),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Array4.flatMap",
    runFunction: run_FlattenAnySeqSeqArray4_flatMap, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Array4.joined",
    runFunction: run_FlattenAnySeqSeqArray4_joined, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Array4.for-in.Elem",
    runFunction: run_FlattenAnySeqSeqArray4_forinElem, tags: t),
  BenchmarkInfo(
    name: "Flatten.AnySeq.Seq.Array4.for-in.Batch",
    runFunction: run_FlattenAnySeqSeqArray4_forinBatch, tags: t),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.flatMap",
    runFunction: run_FlattenArrayColorVal_flatMap, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.map.joined",
    runFunction: run_FlattenArrayColorVal_map_joined, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.lazy.map.joined",
    runFunction: run_FlattenArrayColorVal_lazy_map_joined, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.for-in.Naive",
    runFunction: run_FlattenArrayColorVal_forinNaive, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.Unsafe",
    runFunction: run_FlattenArrayColorValUnsafe, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.Unsafe2",
    runFunction: run_FlattenArrayColorValUnsafe2, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Val.Unsafe3",
    runFunction: run_FlattenArrayColorValUnsafe3, tags: t,
		setUpFunction: { blackHole(arrayColorVals) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Ref.flatMap",
    runFunction: run_FlattenArrayColorRef_flatMap, tags: t,
		setUpFunction: { blackHole(arrayColorRefs) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Ref.map.joined",
    runFunction: run_FlattenArrayColorRef_map_joined, tags: t,
		setUpFunction: { blackHole(arrayColorRefs) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Ref.lazy.map.joined",
    runFunction: run_FlattenArrayColorRef_lazy_map_joined, tags: t,
		setUpFunction: { blackHole(arrayColorRefs) }),
  BenchmarkInfo(
    name: "Flatten.Array.Color.Ref.for-in.Naive",
    runFunction: run_FlattenArrayColorRef_forinNaive, tags: t,
		setUpFunction: { blackHole(arrayColorRefs) }),
]

let size = 1<<14
let flatSize = size * 4
let lastElement = size + 3

let tuple4: (Int) -> (Int, Int, Int, Int) = { ($0, $0 + 1, $0 + 2, $0 + 3) }
let arrayTuple4 = (1...size).map(tuple4)
let sequenceTuple4 = (1...size).lazy.map(tuple4)

let array4: (Int) -> [Int] = { [$0, $0 + 1, $0 + 2, $0 + 3] }
let arrayArray4 = (1...size).map(array4)
let sequenceArray4 = (1...size).lazy.map(array4)

// 32-bit color
struct ColorVal { let r, g, b, a: UInt8 }
final class ColorRef {
  let r, g, b, a: UInt8
  init(r: UInt8, g: UInt8, b: UInt8, a: UInt8) {
    self.r = r; self.g = g; self.b = b; self.a = a
  }
}

let c: (Int) -> UInt8 = { UInt8($0 % 256) } // clamp
let arrayColorVals = (1...size).map {
  ColorVal(r: c($0), g: c($0 + 1), b: c($0 + 2), a: c($0 + 3))
}
let arrayColorRefs = (1...size).map {
  ColorRef(r: c($0), g: c($0 + 1), b: c($0 + 2), a: c($0 + 3))
}
let lastBlue = c(size + 2)

struct CountLast<T> {
  var count: Int = 0
  var last: T? = nil
  static func collect(_ cl: inout CountLast, _ element: T){
    cl.count += 1
    cl.last = element
  }
}

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
public func run_FlattenArrayTuple4_forinNaive(_ N: Int) {
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
public func run_FlattenArrayTuple4Unsafe(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4

    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(to: Int.self, capacity: input.count * 4) {
        Array(UnsafeBufferPointer(start: $0, count: input.count * 4))
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
public func run_FlattenArrayTuple4_lazy_forinNaive(_ N: Int) {
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
public func run_FlattenArrayTuple4_lazy_forinReserve(_ N: Int) {
  for _ in 1...N {
    let input = arrayTuple4.lazy

    var f: [Int] = []
    f.reserveCapacity(input.count * 4)

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
public func run_FlattenSeqTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = sequenceTuple4

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenSeqTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = sequenceTuple4

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenSeqTuple4_forinNaive(_ N: Int) {
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
public func run_FlattenAnySeqArrayTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayTuple4)

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqArrayTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayTuple4)

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqArrayTuple4_forinNaive(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayTuple4)

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
public func run_FlattenAnySeqSeqTuple4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceTuple4)

    let f = input.flatMap { [$0.0, $0.1, $0.2, $0.3] }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqSeqTuple4_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceTuple4)

    let f = input.map({ [$0.0, $0.1, $0.2, $0.3] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqSeqTuple4_forinNaive(_ N: Int) {
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
public func run_FlattenArrayArray4_forinElem(_ N: Int) {
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
public func run_FlattenArrayArray4_forinBatch(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4

    var f: [Int] = []

    for array in input {
      f += array
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
public func run_FlattenArrayArray4_lazy_forinElem(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4.lazy

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
public func run_FlattenArrayArray4_lazy_forinBatch(_ N: Int) {
  for _ in 1...N {
    let input = arrayArray4.lazy

    var f: [Int] = []

    for array in input {
      f += array
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenSeqArray4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    let f = input.flatMap { $0 }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenSeqArray4_joined(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    let f = input.joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenSeqArray4_forinElem(_ N: Int) {
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
public func run_FlattenSeqArray4_forinBatch(_ N: Int) {
  for _ in 1...N {
    let input = sequenceArray4

    var f: [Int] = []

    for array in input {
      f += array
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqArrayArray4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayArray4)

    let f = input.flatMap { $0 }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqArrayArray4_joined(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayArray4)

    let f = input.joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqArrayArray4_forinElem(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayArray4)

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
public func run_FlattenAnySeqArrayArray4_forinBatch(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(arrayArray4)

    var f: [Int] = []

    for array in input {
      f += array
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqSeqArray4_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceArray4)

    let f = input.flatMap { $0 }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqSeqArray4_joined(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceArray4)

    let f = input.joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenAnySeqSeqArray4_forinElem(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceArray4)

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
public func run_FlattenAnySeqSeqArray4_forinBatch(_ N: Int) {
  for _ in 1...N {
    let input = AnySequence(sequenceArray4)

    var f: [Int] = []

    for array in input {
      f += array
    }

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastElement))
  }
}

@inline(never)
public func run_FlattenArrayColorVal_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorVal_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorVal_lazy_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.lazy.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorVal_forinNaive(_ N: Int) {
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
public func run_FlattenArrayColorValUnsafe(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(to: UInt32.self, capacity: input.count) {
        UnsafeBufferPointer(start:$0, count: input.count).flatMap {
          (rawColor: UInt32) -> [UInt8] in
          let rgba = rawColor.bigEndian
          let argb = (rgba << 24) | (rgba >> 8) // rotate
          return stride(from:24, through:0, by: -8).map {
            UInt8(truncatingIfNeeded: argb >> UInt32($0))
          }
        }
      }
    }
    // print(f.suffix(4))

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorValUnsafe2(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.withUnsafeBufferPointer {
      $0.baseAddress!.withMemoryRebound(to: UInt32.self, capacity: input.count) {
        UnsafeBufferPointer(start:$0, count: input.count).map { rgba in
          // rotate (inverted direction, because x86 is little endian)
          (rgba >> 24) | (rgba << 8)
        }
        .withUnsafeBytes(Array.init)
      }
    }
    // print(f.suffix(4))

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorValUnsafe3(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorVals

    let f = input.map { c in ColorVal(r: c.a, g: c.r, b: c.g, a: c.b) }
        .withUnsafeBytes(Array.init)
    // print(f.suffix(4))

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorRef_flatMap(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.flatMap { [$0.a, $0.r, $0.g, $0.b] }

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorRef_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    CheckResults((f.count, f.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorRef_lazy_map_joined(_ N: Int) {
  for _ in 1...N {
    let input = arrayColorRefs

    let f = input.lazy.map({ [$0.a, $0.r, $0.g, $0.b] }).joined()

    let cl = f.reduce(into: CountLast(), CountLast.collect)
    CheckResults((cl.count, cl.last!) == (flatSize, lastBlue))
  }
}

@inline(never)
public func run_FlattenArrayColorRef_forinNaive(_ N: Int) {
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


// Local Variables:
// eval: (read-only-mode 1)
// End:

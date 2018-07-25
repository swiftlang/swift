//===--- ArrayAppend.swift ------------------------------------------------===//
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


import TestsUtils

// This benchmark tests closureless versions of min and max, both contains,
// repeatElement and reduce, on a number of different sequence types.
// To avoid too many little micro benchmarks, it measures them all together
// for each sequence type.

public let SequenceAlgos = [
  BenchmarkInfo(name: "SequenceAlgosList", runFunction: run_SequenceAlgosList, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
  BenchmarkInfo(name: "SequenceAlgosArray", runFunction: run_SequenceAlgosArray, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
  BenchmarkInfo(name: "SequenceAlgosContiguousArray", runFunction: run_SequenceAlgosContiguousArray, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
  BenchmarkInfo(name: "SequenceAlgosRange", runFunction: run_SequenceAlgosRange, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
  BenchmarkInfo(name: "SequenceAlgosUnfoldSequence", runFunction: run_SequenceAlgosUnfoldSequence, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
  BenchmarkInfo(name: "SequenceAlgosAnySequence", runFunction: run_SequenceAlgosAnySequence, tags: [.validation, .api], setUpFunction: { buildWorkload() }, tearDownFunction: nil),
]

extension List: Sequence {
  struct Iterator: IteratorProtocol {
    var _list: List<Element>
    mutating func next() -> Element? {
      guard case let .node(x,xs) = _list else { return nil }
      _list = xs
      return x
    }
  }
  func makeIterator() -> Iterator {
    return Iterator(_list: self)
  }
}

extension List: Equatable where Element: Equatable {
  static func == (lhs: List<Element>, rhs: List<Element>) -> Bool {
    return lhs.elementsEqual(rhs)
  }
}

func benchmarkSequenceAlgos<S: Sequence>(s: S, n: Int) where S.Element == Int {
  CheckResults(s.reduce(0, &+) == (n*(n-1))/2)
  let mn = s.min()
  let mx = s.max()
  CheckResults(mn == 0 && mx == n-1)
  CheckResults(s.starts(with: s))
}

let n = 10_000
let r = 0..<(n*100)
let l = List(0..<n)
let c = ContiguousArray(0..<(n*100))
let a = Array(0..<(n*100))
let y = AnySequence(0..<n)
let s = sequence(first: 0, next: { $0 < n&-1 ? $0&+1 : nil})

func buildWorkload() {
  blackHole(l.makeIterator())
  blackHole(c.makeIterator())
  blackHole(a.makeIterator())
  blackHole(y.makeIterator())
  blackHole(s.makeIterator())
}

func benchmarkEquatableSequenceAlgos<S: Sequence>(s: S, n: Int) where S.Element == Int, S: Equatable {
  CheckResults(repeatElement(s, count: 1).contains(s))
  CheckResults(!repeatElement(s, count: 1).contains { $0 != s })
}

@inline(never)
public func run_SequenceAlgosRange(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: r, n: r.count)
    benchmarkEquatableSequenceAlgos(s: r, n: r.count)
  }
}

@inline(never)
public func run_SequenceAlgosArray(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: a, n: a.count)
    benchmarkEquatableSequenceAlgos(s: a, n: a.count)
  }
}

@inline(never)
public func run_SequenceAlgosContiguousArray(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: c, n: c.count)
    benchmarkEquatableSequenceAlgos(s: c, n: c.count)
  }
}

@inline(never)
public func run_SequenceAlgosAnySequence(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: y, n: n)
  }
}

@inline(never)
public func run_SequenceAlgosUnfoldSequence(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: s, n: n)
  }
}

@inline(never)
public func run_SequenceAlgosList(_ N: Int) {
  for _ in 0..<N {
    benchmarkSequenceAlgos(s: l, n: n)
    benchmarkEquatableSequenceAlgos(s: l, n: n)
  }
}

enum List<Element> {
  case end
  indirect case node(Element, List<Element>)
  
  init<S: BidirectionalCollection>(_ elements: S) where S.Element == Element {
    self = elements.reversed().reduce(.end) { .node($1,$0) }
  }
}


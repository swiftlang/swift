// DictOfArraysToArrayOfDicts benchmark
//
// Description: Convert a dictionary of [key: [values]] to an array of
//              dictionaries [[key: value]] using zipWith.
// Source: https://gist.github.com/airspeedswift/3675952127ee775551b0

import TestsUtils

public var DictOfArraysToArrayOfDicts = BenchmarkInfo(
  name: "DictOfArraysToArrayOfDicts",
  runFunction: run_DictOfArraysToArrayOfDicts,
  tags: [.algorithm, .Dictionary]
)

@inline(never)
public func run_DictOfArraysToArrayOfDicts(_ N: Int) {
  let returnedFromServer = [
    "title": ["abc",  "def", "ghi"],
    "time": ["1234", "5678", "0123"],
    "content": ["qwerty", "asdfg", "zxcvb"],
  ]
  var pairs: [[(String, String)]] = []
  var inverted: [[(String, String)]] = []
  var arrayOfDicts: [[String: String]] = [[:]]

  for _ in 1...100*N {
    pairs = returnedFromServer.map {
      (key, value) in value.map { (key, $0) }
    }
    inverted = zipWith(pairs[0], pairs[1], pairs[2]) {
      [$0] + [$1] + [$2]
    }
    arrayOfDicts = inverted
    .map { $0.map { (key: $0.0, value: $0.1) } }
    .map { Dictionary($0) }

    if !(arrayOfDicts.count == 3) {
      break
    }
  }

  CheckResults(arrayOfDicts.count == 3)
}

// Given [
//  "title" : ["abc",  "def"],
//  "time"  : ["1234", "5678", "0123"],
//  "content":["qwerty", "asdfg", "zxcvb"]
// ]
//
// how do you get to this:
//
// [
//  ["title" : "abc",
//   "time"  : "1234",
//   "content": "qwerty"],
// ["title" : "def",
//  "time"  : "5678",
//  "content": "asdfg"],
// ["title" : "ghi",
//  "time"  : "0123",
//  "content": "zxcvb"]]

public func zip3 <A: Sequence,
B: Sequence,
C: Sequence> (_ a: A, _ b: B, _ c: C)
-> ZipSequence3<A, B, C> {
  return ZipSequence3(a, b, c)
}

// Sequence of tuples created from values from three other sequences
public struct ZipSequence3<A: Sequence,
B: Sequence,
C: Sequence>: Sequence {
  public typealias Iterator = ZipGenerator3
  <A.Iterator, B.Iterator, C.Iterator>
  public typealias SubSequence = AnySequence<Iterator.Element>

  private var a: A
  private var b: B
  private var c: C

  public init (_ a: A, _ b: B, _ c: C) {
    self.a = a
    self.b = b
    self.c = c
  }

  public func makeIterator() -> Iterator {
    return ZipGenerator3(a.makeIterator(), b.makeIterator(), c.makeIterator())
  }
}

// Iterator that creates tuples of values from three other generators
public struct ZipGenerator3<A: IteratorProtocol,
B: IteratorProtocol,
C: IteratorProtocol>: IteratorProtocol {
  private var a: A
  private var b: B
  private var c: C

  public init(_ a: A, _ b: B, _ c: C) {
    self.a = a
    self.b = b
    self.c = c
  }

  mutating public func next() -> (A.Element, B.Element, C.Element)? {
    switch (a.next(), b.next(), c.next()) {
    case let (.some(aValue), .some(bValue), .some(cValue)):
      return (aValue, bValue, cValue)
    default:
      return nil
    }
  }
}

func zipWith
<S1: Sequence, S2: Sequence, S3: Sequence, T>
(_ s1: S1, _ s2: S2, _ s3: S3, _ combine: (S1.Iterator.Element,
    S2.Iterator.Element,
    S3.Iterator.Element) -> T) -> [T] {
  return zip3(s1,s2,s3).map(combine)
}

extension Dictionary {
  // Construct from an arbitrary sequence with elements of the tupe
  // `(Key,Value)`
  init<S: Sequence> (_ seq: S) where S.Iterator.Element == Element {
    self.init()
    self.merge(seq)
  }

  // Merge a sequence of `(Key,Value)` tuples into the dictionary
  mutating func merge<S: Sequence> (_ seq: S) where S.Iterator.Element == Element {
    var gen = seq.makeIterator()
    while let (k, v) = gen.next() {
      self[k] = v
    }
  }
}

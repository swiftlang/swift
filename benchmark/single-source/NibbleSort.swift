// NibbleSort benchmark
//
// Source: https://gist.github.com/airspeedswift/f4daff4ea5c6de9e1fdf

import TestsUtils

public var NibbleSort = BenchmarkInfo(
  name: "NibbleSort",
  runFunction: run_NibbleSort,
  tags: [.validation]
)

@inline(never)
public func run_NibbleSort(_ N: Int) {
  let vRef: UInt64 = 0xfeedbba000000000
  let v: UInt64 = 0xbadbeef
  var c = NibbleCollection(v)

  for _ in 1...10000*N {
    c.val = v
    c.sort()

    if c.val != vRef {
      break
    }
  }

  CheckResults(c.val == vRef)
}

struct NibbleCollection: RandomAccessCollection, MutableCollection {
  var val: UInt64
  init(_ val: UInt64) { self.val = val }

  typealias Index = UInt64
  let startIndex: UInt64 = 0
  let endIndex: UInt64 = 16

  subscript(bounds: Range<Index>) -> Slice<NibbleCollection> {
    get {
      fatalError("Should not be called. Added here to please the type checker")
    }
    set {
      fatalError("Should not be called. Added here to please the type checker")
    }
  }

  subscript(idx: UInt64) -> UInt64 {
    get {
      return (val >> (idx*4)) & 0xf
    }
    set(n) {
      let mask = (0xf as UInt64) << (idx * 4)
      val &= ~mask
      val |= n << (idx * 4)
    }
  }

  typealias Iterator = IndexingIterator<NibbleCollection>
  func makeIterator() -> Iterator { return Iterator(_elements: self) }
}

// RUN: %target-swift-frontend -emit-sil %s -enable-experimental-feature Embedded -wmo -verify

// REQUIRES: swift_feature_Embedded

@inline(never)
func sink<T: BinaryInteger>(_ n: T) {}

@inline(never)
func mayThrow<T: BinaryInteger>(_ n: T) throws {}

public func testPackExpansion<each T: BinaryInteger>(_ ts: repeat each T) throws {
  _ = (repeat sink(each ts)) // okay, no early exit or branch
  _ = try (repeat mayThrow(each ts)) // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
}

public func normalPackLoop<each T: BinaryInteger>(_ ts: repeat each T) {
  for t in repeat each ts { // okay, no early exit or branch
    sink(t)
  }

}

public func breakFromPackLoop<each T: BinaryInteger>(_ ts: repeat each T) {
  for t in repeat each ts { // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
    if t.bitWidth == 16 { break }
    sink(t)
  }
}

public func returnFromPackLoop<each T: BinaryInteger>(_ ts: repeat each T) -> Int {
  for t in repeat each ts { // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
    if t.bitWidth == 16 { return 1 }
  }
  return 0
}

public func throwFromPackLoop<each T: BinaryInteger>(_ ts: repeat each T) throws {
  for t in repeat each ts { // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
    try mayThrow(t)
    sink(t)
  }
}

public func continueInPackLoop<each T: BinaryInteger>(_ ts: repeat each T) {
  for t in repeat each ts { // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
    if t.bitWidth == 16 { continue }
    sink(t)
  }
}

public func ifInPackLoop<each T: BinaryInteger>(_ ts: repeat each T) {
  for t in repeat each ts { // expected-error{{cannot specialize pack expression or statement in embedded Swift. This is a compiler bug. Please file a bug with a small example of the bug}}
    if t.bitWidth == 16 { sink(0) }
    sink(t)
  }
}

public func callPackFunctions() {
  try! testPackExpansion(Int8(1), Int16(2), Int32(3)) // expected-note{{generic specialization called here}}
  normalPackLoop(Int8(1), Int16(2), Int32(3))
  breakFromPackLoop(Int8(1), Int16(2), Int32(3))
  print(returnFromPackLoop(Int8(1), Int16(2), Int32(3)))
  try! throwFromPackLoop(Int8(1), Int16(2), Int32(3))
  continueInPackLoop(Int8(1), Int16(2), Int32(3))
  ifInPackLoop(Int8(1), Int16(2), Int32(3))
}

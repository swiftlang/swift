// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Check that the compiled code does not crash because of an over-release.
// End-to-end test for rdar://problem/48906146.

struct LCRNG: RandomNumberGenerator {
  private var state: UInt64

  init(seed: Int) {
    state = UInt64(truncatingIfNeeded: seed)
    for _ in 0..<10 { _ = next() }
  }

  mutating func next() -> UInt64 {
    state = 2862933555777941757 &* state &+ 3037000493
    return state
  }
}


func test(_ body: () -> Void) {
  body()
}

test {
  var rng = LCRNG(seed: 42)
  let v = Array(0..<3).shuffled(using: &rng)
  // CHECK: 3
  print(v.count)
}


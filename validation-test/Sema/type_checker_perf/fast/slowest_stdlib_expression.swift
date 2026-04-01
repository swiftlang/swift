// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200

// This was the slowest expression in the standard library at the time.
// It was fast in 6.2, and got slower in 6.3.

// 6.2: 38 scopes
// 6.3: 1223 scopes
// now: 162 scopes

struct S {
  var value: UInt

  func f(_ bit: Int) -> Bool {
    // &, &<<, and != have many overloads.
    return value & (1 &<< bit) != 0
  }
}

// RUN: %target-swift-frontend -parse-as-library -O -primary-file %s -sil-verify-all -emit-sil -o /dev/null

// rdar://188958548
// Check that LoopRotate does not create an owned phi for a debug_value that
// follows the consume of its operand. See
// test/SILOptimizer/looprotate_nontrivial_ossa.sil.

@inline(never) func opaque() -> Bool { true }

struct S: Equatable {
  var a: [Int64] = []
  var check: Bool { opaque() }
  static func ==(lhs: S, rhs: S) -> Bool {
    if lhs.a != rhs.a { return false }
    if !lhs.check || !rhs.check { return false }
    return true
  }
}

func f(a: borrowing [S], b: borrowing [S]) -> Bool { a == b }

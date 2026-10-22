// RUN: not --crash %target-swift-frontend -emit-sil -sil-verify-all %s

// A `switch consume` over a loadable non-Copyable enum with a `case is` whose
// cast is statically known to fail. SILGen emits one `begin_borrow [fixed]` of
// the subject that spans the `checked_cast_addr_br test_only` and the
// `switch_enum`. MoveOnlyChecker then re-creates that borrow only where the
// `switch_enum` uses it, but leaves the `end_borrow` on the cast's success path,
// so SIL verification fails with:
//
//   instruction isn't dominated by its operand: properlyDominates(valueI, I)
//   ->   end_borrow %25 : $b
//
// This is basically the same as SILGenFunction-emitSwitchStmt-e5fd43
// just crashing at a later point.

struct a {}

enum b: ~Copyable {
  case c
}

func d(e: consuming b) -> Int {
  switch consume e {
  case is a:
    return 1
  case .c:
    return 2
  }
}

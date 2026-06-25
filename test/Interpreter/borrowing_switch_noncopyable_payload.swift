// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// Regression test for a missed-copy bug in the move-only object checker.
// The cleanup that erases the SILGen-introduced `copy_value` feeding into
// a `mark_unresolved_non_copyable_value [strict] [no_consume_or_assign]`
// (the `bindBorrow` "notional copy" pattern) only looked through a fixed
// set of producers when locating the underlying `@guaranteed` source.
// `destructure_tuple` was missing from that set, so a `let l, let r`
// binding under a `borrowing` switch over a `~Copyable` enum case with a
// tuple payload would leave the copy_value alive after move-only checking,
// at which point the missed-copy detector reported the generic compiler
// bug diagnostic.
//
// Single-payload bindings (`case .single(let b)`) were unaffected because
// the source there is the `@guaranteed` block argument directly.

struct Box: ~Copyable {
  var x: Int
}

enum E: ~Copyable {
  case empty
  case single(Box)
  case pair(Box, Box)
}

func sum(_ e: borrowing E) -> Int {
  switch e {
  case .empty:
    return 0
  case .single(let b):
    return b.x
  case .pair(let l, let r):
    return l.x + r.x
  }
}

precondition(sum(.empty) == 0)
precondition(sum(.single(Box(x: 42))) == 42)
precondition(sum(.pair(Box(x: 3), Box(x: 4))) == 7)

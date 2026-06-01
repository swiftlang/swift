// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sil %s

// Regression test for the following cases:

// SILGen emits the `consume q` operator on a `let` binding of a generic
// `~Copyable` type as an SSA chain:
//
//     %loaded = load [copy] %q_storage
//     %moved  = move_value [allows_diagnostics] %loaded
//     %mark   = mark_unresolved_non_copyable_value [consumable_and_assignable]
//                                                  %moved
//
// Under opaque values, AddressLowering materializes this into a fresh
// `alloc_stack` initialized by `copy_addr [init]` and then marked. The
// resulting mark sits on a slot whose initialization writes to the slot
// itself rather than through the mark, so MoveOnlyAddressChecker's
// `GatherUsesVisitor` (walking transitively from the mark) can't reach
// the init via the use-chain.

struct U<T>: ~Copyable {
  var x: T
}

func test<T>(_ a: T) {
  let q = U<T>(x: a)
  _ = consume q
}

// SILGen emits a +1 owned form for `borrowing` self of `@noImplicitCopy`
// loadable copyable types so the move-only object checker can analyze the
// mark on owned storage:
//
//     %wrap = copyable_to_moveonlywrapper [guaranteed] %self
//     %copy = copy_value %wrap
//     %mark = mark_unresolved_non_copyable_value [no_consume_or_assign] %copy
//     ... uses ...
//     destroy_value %mark
//
// Under opaque values, AddressLowering recognizes this `copy_value → mark
// [no_consume_or_assign]` pattern with a guaranteed source and projects both
// onto the source's storage, erasing the artificial `destroy_value` cleanup.
// Without that projection the destroy_value would either lower to a
// `destroy_addr` of borrowed storage (illegal SIL) or survive as a consuming
// boundary use of the +1 owned mark and trip the move-only address checker's
// `[no_consume_or_assign]` diagnostic.

struct Loaner<T> {
  let value: T
  var property: T { borrowing get { value } }
  borrowing func method() -> T { value }
}

// Passing a stored `~Copyable` value as a `borrowing` argument produces, under
// opaque values, a value-form copy that AddressLowering reifies into
// `alloc_stack` + `copy_addr [init]` immediately before a
// `mark_unresolved_non_copyable_value [no_consume_or_assign]`. The move-only
// address checker could not establish def-initialization for that mark and
// hit the assertion.

struct NC: ~Copyable {
  var x: Int = 0
}

final class Holder {
  var inner: NC
  init(i: consuming NC) { self.inner = i }
}

func borrowMe<T: ~Copyable>(_ n: borrowing T) {}

func caller(_ h: Holder) {
  borrowMe(h.inner)
}

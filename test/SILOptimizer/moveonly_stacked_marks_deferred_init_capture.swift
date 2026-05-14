// RUN: %target-swift-frontend -emit-sil -verify %s

// Source-level regression test: a noncopyable `let` with deferred
// conditional initialization captured by a non-escaping closure
// previously crashed the move-only checker with
//
//     Assertion failed: (isInitialized()), function finishedInitializationOfDefs
//
// because SILGen emits stacked `mark_unresolved_non_copyable_value`
// instructions on the captured `let`'s alloc_stack, which none of the
// special cases in `addressBeginsInitialized` recognized. The
// assertion now treats the stacked-mark pattern as beginning-
// initialized.
//
// A separate SILGen issue still emits illegal `copy_value` of the
// captured noncopyable value and surfaces here as
// `sil_movechecking_bug_missed_copy`; once that is fixed this test
// should be updated to expect a clean compile. The important
// property anchored here is that the compiler emits a clean
// diagnostic instead of asserting.

struct NC: ~Copyable {
    func f() { }
}

func take(_: () -> ()) { }

func deferredInitClosureCapture() {
    let nc: NC // expected-error {{copy of noncopyable typed value. This is a compiler bug.}}
    if .random() {
        nc = .init()
    } else {
        nc = .init()
    }
    take { nc.f() }
}

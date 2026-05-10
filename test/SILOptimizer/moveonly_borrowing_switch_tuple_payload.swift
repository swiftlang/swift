// RUN: %target-swift-frontend -emit-sil -verify %s

// Negative coverage for the `bindBorrow` tuple-payload destructure fix in
// `MoveOnlyObjectCheckerUtils.cpp::eraseMarkWithCopiedOperand`. After the
// fix, the SILGen-introduced `copy_value` for each destructured component
// is correctly elided when the binding is genuinely borrowed, but the
// underlying `mark_unresolved_non_copyable_value [no_consume_or_assign]`
// must still reject any actual consume of the binding.
//
// Pins the proper "borrowed and cannot be consumed" + "consumed here"
// diagnostic pair (rather than a regression to the catch-all "compiler
// bug" missed-copy error or no diagnostic at all) for body-consume of
// tuple-destructured `let l, let r` bindings.
//
// (A separate, pre-existing diagnostic-quality bug causes consumes in the
// `where` clause of a tuple-destructured case to fall through to the
// catch-all "escaping closure" diagnostic for the second binding; not
// covered here because it is unrelated to this fix.)

struct Box: ~Copyable { var x: Int }

enum Pair: ~Copyable {
    case both(Box, Box)
}

func eat(_: consuming Box) {}
func borrow(_: borrowing Box) {}

// Borrow-only uses must compile cleanly. Without the fix, this would
// regress to the missed-copy "compiler bug" diagnostic on each binding.
func borrowOnly(_ p: borrowing Pair) {
    switch p {
    case .both(let l, let r):
        borrow(l)
        borrow(r)
    }
}

// Wildcard binding still emits `destructure_tuple` for the case payload
// and copies just the first element. Without the destructure look-through,
// this case would also regress to the missed-copy diagnostic.
func wildcardBinding(_ p: borrowing Pair) {
    switch p {
    case .both(let l, _):
        borrow(l)
    }
}

// Consuming both destructured components in the body must report the
// proper "borrowed and cannot be consumed" + "consumed here" diagnostic
// for each binding.
func consumeBothInBody(_ p: borrowing Pair) {
    switch p {
    case .both(let l, // expected-error{{'l' is borrowed and cannot be consumed}}
               let r): // expected-error{{'r' is borrowed and cannot be consumed}}
        eat(l) // expected-note{{consumed here}}
        eat(r) // expected-note{{consumed here}}
    }
}

// Mixed: consuming one element while borrowing the other must produce
// the diagnostic only for the consumed binding, leaving the borrowed
// binding alone.
func consumeOneBorrowOther(_ p: borrowing Pair) {
    switch p {
    case .both(let l, // expected-error{{'l' is borrowed and cannot be consumed}}
               let r):
        eat(l) // expected-note{{consumed here}}
        borrow(r)
    }
}

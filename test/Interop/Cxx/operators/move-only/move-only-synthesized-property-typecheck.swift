// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=swift-5.9 -DNO_CONSUME
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=swift-6 -DNO_CONSUME
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -DNO_CONSUME

// Note: some errors need full SIL emission right now.
// FIXME: they should be type checker errors.
// RUN: not %target-swift-emit-sil %s -I %S/Inputs -cxx-interoperability-mode=swift-6 2>&1 | %FileCheck %s
// RUN: not %target-swift-emit-sil %s -I %S/Inputs -cxx-interoperability-mode=upcoming-swift 2>&1 | %FileCheck %s

import MoveOnlyCxxOperators

func borrowNC(_ x: borrowing NonCopyable) -> CInt {
  return x.method(3)
}

func inoutNC(_ x: inout NonCopyable) -> CInt {
  return x.mutMethod(5)
}

func consumingNC(_ x: consuming NonCopyable) {
  // do nothing.
}

func testNonCopyableHolderConstDerefPointee() {
    var holder = NonCopyableHolderConstDeref(11)
    _ = borrowNC(holder.pointee) // ok
    _ = holder.pointee.method(2)
    _ = holder.pointee.x
#if NO_CONSUME
    _ = inoutNC(holder.pointee)  // expected-error {{cannot pass immutable value as inout argument: 'pointee' is a get-only property}}
    holder.pointee.mutMethod(1) // expected-error {{cannot use mutating member on immutable value: 'pointee' is a get-only property}}
    holder.pointee.x = 2 // expected-error {{cannot assign to property: 'pointee' is a get-only property}}
#else
    consumingNC(holder.pointee) // CHECK-DAG: [[@LINE]]:{{.*}}: error:
    let consumeVal = holder.pointee // CHECK-DAG: [[@LINE]]:{{.*}}: error:
#endif
}

func testNonCopyableHolderPairedDerefPointee() {
    var holder = NonCopyableHolderPairedDeref(11)
    _ = borrowNC(holder.pointee) // ok
    _ = holder.pointee.method(2)
    _ = holder.pointee.x
    _ = inoutNC(&holder.pointee)
    _ = holder.pointee.mutMethod(1)
    holder.pointee.x = 2
    consumingNC(holder.pointee)
    let consumeVal = holder.pointee
    _ = borrowNC(consumeVal)
}

func testNonCopyableHolderMutDerefPointee() {
    var holder = NonCopyableHolderMutDeref(11)
    _ = borrowNC(holder.pointee) // ok
    _ = holder.pointee.method(2)
    _ = holder.pointee.x
    _ = inoutNC(&holder.pointee)
    _ = holder.pointee.mutMethod(1)
    holder.pointee.x = 2
    consumingNC(holder.pointee)
    let consumeVal = holder.pointee
    _ = borrowNC(consumeVal)
}

func testNonCopyableHolderMutDerefPointeeLet() {
#if NO_CONSUME
    let holder = NonCopyableHolderMutDeref(11) // expected-note {{}}
    _ = borrowNC(holder.pointee) // expected-error {{cannot use mutating getter on immutable value: 'holder' is a 'let' constant}}
#endif
}

func testNonCopyableHolderValueConstDerefPointeeLet() {
    let holder = NonCopyableHolderValueConstDeref(11)
    let val = holder.pointee // expected-note {{}}
    _ = borrowNC(val) // ok
#if NO_CONSUME
    val.mutMethod(3) // expected-error {{cannot use mutating member on immutable value: 'val' is a 'let' constant}}
#endif
}

func testNonCopyableHolderValueMutDerefPointeeLet() {
    var holder = NonCopyableHolderValueMutDeref(11)
    let val = holder.pointee // expected-note {{}}
    _ = borrowNC(val) // ok
#if NO_CONSUME
    val.mutMethod(3) // expected-error {{cannot use mutating member on immutable value: 'val' is a 'let' constant}}
#endif
}

testNonCopyableHolderConstDerefPointee()
testNonCopyableHolderPairedDerefPointee()
testNonCopyableHolderMutDerefPointee()
testNonCopyableHolderMutDerefPointeeLet()
testNonCopyableHolderValueConstDerefPointeeLet()
testNonCopyableHolderValueMutDerefPointeeLet()

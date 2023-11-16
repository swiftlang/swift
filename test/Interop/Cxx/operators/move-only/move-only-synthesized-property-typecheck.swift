// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -DNO_CONSUME

// Note: some errors need full SIL emission right now.
// FIXME: they should be type checker errors.
// RUN: not %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

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
    consumingNC(holder.pointee) // CHECK: [[@LINE]]:{{.*}}: error:
    let consumeVal = holder.pointee // CHECK: [[@LINE]]:{{.*}}: error:
#endif
}

testNonCopyableHolderConstDerefPointee()

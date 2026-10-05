// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xllvm -sil-disable-pass=mandatory-temp-rvalue-elimination -Xfrontend -enable-experimental-cxx-interop -Onone) | %FileCheck -check-prefix=CHECK-DASH-ONONE %s
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -O) | %FileCheck -check-prefix=CHECK-DASH-O %s
//
// REQUIRES: executable_test
// REQUIRES: stdlib_5_8_runtime
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import SpecialMembers

func consume(_ x: consuming MoveOnly) {}
func consume(_ x: consuming Copyable) {}

func moveOnly1() {
    let x = MoveOnly()
// CHECK-DASH-ONONE: MoveOnly 0 created
// CHECK-DASH-O: MoveOnly 0 created
    byRValueRef(consuming: x)
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 move-created
// CHECK-DASH-O-NEXT: MoveOnly 1 move-created
// CHECK-DASH-ONONE-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-O-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 destroyed
// CHECK-DASH-O-NEXT: MoveOnly 1 destroyed
}

func moveOnly2() {
    let x = MoveOnly()
// CHECK-DASH-ONONE-NEXT: MoveOnly 0 created
// CHECK-DASH-O-NEXT: MoveOnly 0 created
    consume(x)
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 move-created
// CHECK-DASH-ONONE-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-O-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-ONONE-NEXT: MoveOnly 2 move-created
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 destroyed
// CHECK-DASH-ONONE-NEXT: MoveOnly 2 destroyed
}

func moveOnly3() {
    var u = UniversalMoveOnly.init()
    let x = MoveOnly()
// CHECK-DASH-ONONE: MoveOnly 0 created
// CHECK-DASH-O: MoveOnly 0 created
    u.byRValueRef(consuming: x)
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 move-created
// CHECK-DASH-O-NEXT: MoveOnly 1 move-created
// CHECK-DASH-ONONE-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-O-NEXT: MoveOnly 0 destroyed
// CHECK-DASH-ONONE-NEXT: MoveOnly 1 destroyed
// CHECK-DASH-O-NEXT: MoveOnly 1 destroyed
}

func copyable1() {
    let x = Copyable()
// CHECK-DASH-ONONE-NEXT: Copyable 0 created
// CHECK-DASH-O-NEXT: Copyable 0 created
    byRValueRef(consuming: x)
// CHECK-DASH-ONONE-NEXT: Copyable 1 copy-created
// CHECK-DASH-O-NEXT: Copyable 1 copy-created
// CHECK-DASH-ONONE-NEXT: Copyable 1 destroyed
// CHECK-DASH-O-NEXT: Copyable 1 destroyed
// CHECK-DASH-ONONE-NEXT: Copyable 0 destroyed
// CHECK-DASH-O-NEXT: Copyable 0 destroyed
}

func copyable2() {
    let x4 = Copyable()
// CHECK-DASH-ONONE-NEXT: Copyable 0 created
// CHECK-DASH-O-NEXT: Copyable 0 created
    consume(x4)
// CHECK-DASH-ONONE-NEXT: Copyable 1 copy-created
// CHECK-DASH-ONONE-NEXT: Copyable 2 move-created
// CHECK-DASH-ONONE-NEXT: Copyable 1 destroyed
// CHECK-DASH-ONONE-NEXT: Copyable 2 destroyed
// CHECK-DASH-ONONE-NEXT: Copyable 0 destroyed
// CHECK-DASH-O-NEXT: Copyable 0 destroyed
}

func copyable3() {
    var u = UniversalCopyable.init()
    let x = Copyable()
// CHECK-DASH-ONONE-NEXT: Copyable 0 created
// CHECK-DASH-O-NEXT: Copyable 0 created
    u.byRValueRef(consuming: x)
// CHECK-DASH-ONONE-NEXT: Copyable 1 copy-created
// CHECK-DASH-O-NEXT: Copyable 1 copy-created
// CHECK-DASH-ONONE-NEXT: Copyable 1 destroyed
// CHECK-DASH-O-NEXT: Copyable 1 destroyed
// CHECK-DASH-ONONE-NEXT: Copyable 0 destroyed
// CHECK-DASH-O-NEXT: Copyable 0 destroyed
}

// An rvalue-qualified 'this' is passed the same way as an rvalue-reference
// parameter: the callee gets a pointer to an object the caller destroys.
func rvalueThis1() {
    let x = RValueThis()
// CHECK-DASH-ONONE-NEXT: RValueThis 0 created
// CHECK-DASH-O-NEXT: RValueThis 0 created
    x.onRValue()
// CHECK-DASH-ONONE-NEXT: RValueThis 1 copy-created
// CHECK-DASH-O-NEXT: RValueThis 1 copy-created
// CHECK-DASH-ONONE-NEXT: RValueThis 1 onRValue
// CHECK-DASH-O-NEXT: RValueThis 1 onRValue
// CHECK-DASH-ONONE-NEXT: RValueThis 1 destroyed
// CHECK-DASH-O-NEXT: RValueThis 1 destroyed
// CHECK-DASH-ONONE-NEXT: RValueThis 0 destroyed
// CHECK-DASH-O-NEXT: RValueThis 0 destroyed
}

// A temporary receiver needs no copy at all.
func rvalueThis2() {
    RValueThis().onRValue()
// CHECK-DASH-ONONE-NEXT: RValueThis 0 created
// CHECK-DASH-O-NEXT: RValueThis 0 created
// CHECK-DASH-ONONE-NEXT: RValueThis 0 onRValue
// CHECK-DASH-O-NEXT: RValueThis 0 onRValue
// CHECK-DASH-ONONE-NEXT: RValueThis 0 destroyed
// CHECK-DASH-O-NEXT: RValueThis 0 destroyed
}

// Overloading on the ref-qualifier renames the rvalue one; the lvalue overload
// borrows, the rvalue one takes its own object.
func rvalueThis3() {
    let x = RefQualifiedThis()
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 0 created
// CHECK-DASH-O-NEXT: RefQualifiedThis 0 created
    x.method()
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 0 lvalue
// CHECK-DASH-O-NEXT: RefQualifiedThis 0 lvalue
    x.methodMutatingConsuming()
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 1 copy-created
// CHECK-DASH-O-NEXT: RefQualifiedThis 1 copy-created
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 1 rvalue
// CHECK-DASH-O-NEXT: RefQualifiedThis 1 rvalue
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 1 destroyed
// CHECK-DASH-O-NEXT: RefQualifiedThis 1 destroyed
// CHECK-DASH-ONONE-NEXT: RefQualifiedThis 0 destroyed
// CHECK-DASH-O-NEXT: RefQualifiedThis 0 destroyed
}

// A move-only receiver is moved out of, not copied.
func rvalueThis4() {
    let x = MoveOnlyThis()
// CHECK-DASH-ONONE-NEXT: MoveOnlyThis 0 created
// CHECK-DASH-O-NEXT: MoveOnlyThis 0 created
    x.onRValue()
// CHECK-DASH-ONONE-NEXT: MoveOnlyThis 1 move-created
// CHECK-DASH-O-NEXT: MoveOnlyThis 1 move-created
// CHECK-DASH-ONONE-NEXT: MoveOnlyThis 0 destroyed
// CHECK-DASH-O-NEXT: MoveOnlyThis 0 destroyed
// CHECK-DASH-ONONE-NEXT: MoveOnlyThis 1 onRValue
// CHECK-DASH-O-NEXT: MoveOnlyThis 1 onRValue
// CHECK-DASH-ONONE-NEXT: MoveOnlyThis 1 destroyed
// CHECK-DASH-O-NEXT: MoveOnlyThis 1 destroyed
}

// A type imported as a class is itself the reference: 'this' is passed directly,
// with nothing copied. The count is real, so an unbalanced release would trap,
// and the object is destroyed exactly once.
@available(SwiftStdlib 5.8, *)
func rvalueThis5() {
    let r = RefThis()
// CHECK-DASH-ONONE-NEXT: RefThis created
// CHECK-DASH-O-NEXT: RefThis created
    r.onRValue()
// CHECK-DASH-ONONE-NEXT: RefThis onRValue
// CHECK-DASH-O-NEXT: RefThis onRValue
// CHECK-DASH-ONONE-NEXT: RefThis destroyed
// CHECK-DASH-O-NEXT: RefThis destroyed
}

func main() {
    moveOnly1()
    moveOnly2()
    moveOnly3()
    copyable1()
    copyable2()
    copyable3()
    rvalueThis1()
    rvalueThis2()
    rvalueThis3()
    rvalueThis4()
    if #available(SwiftStdlib 5.8, *) {
        rvalueThis5()
    }
}

main()

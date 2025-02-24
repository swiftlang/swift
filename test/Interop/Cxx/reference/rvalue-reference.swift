// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop) | %FileCheck %s
//
// REQUIRES: executable_test

import SpecialMembers

func consume(_ x: consuming MoveOnly) {}
func consume(_ x: consuming Copyable) {}

func main() {
    let x = MoveOnly()
// CHECK: MoveOnly 0 created
    byRValueRef(consuming: x)
// CHECK-NEXT: MoveOnly 1 move-created
// CHECK-NEXT: MoveOnly 0 destroyed
// CHECK-NEXT: MoveOnly 1 destroyed
    let x2 = MoveOnly()
// CHECK-NEXT: MoveOnly 0 created
    consume(x2)
// CHECK-NEXT: MoveOnly 1 move-created
// CHECK-NEXT: MoveOnly 0 destroyed
// CHECK-NEXT: MoveOnly 2 move-created
// CHECK-NEXT: MoveOnly 1 destroyed
// CHECK-NEXT: MoveOnly 2 destroyed
    let x3 = Copyable()
// CHECK-NEXT: Copyable 0 created
    byRValueRef(consuming: x3)
// CHECK-NEXT: Copyable 1 copy-created
// CHECK-NEXT: Copyable 1 destroyed
    let x4 = Copyable()
// CHECK-NEXT: Copyable 0 created
    consume(x4)
// CHECK-NEXT: Copyable 1 copy-created
// CHECK-NEXT: Copyable 2 move-created
// CHECK-NEXT: Copyable 1 destroyed
// CHECK-NEXT: Copyable 2 destroyed
// CHECK-NEXT: Copyable 0 destroyed
// CHECK-NEXT: Copyable 0 destroyed
}

main()

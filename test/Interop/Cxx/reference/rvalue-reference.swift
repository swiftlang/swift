// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Onone) | %FileCheck -check-prefix=CHECK-DASH-ONONE %s
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -O) | %FileCheck -check-prefix=CHECK-DASH-O %s
//
// REQUIRES: executable_test

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

func main() {
    moveOnly1()
    moveOnly2()
    copyable1()
    copyable2()
}

main()

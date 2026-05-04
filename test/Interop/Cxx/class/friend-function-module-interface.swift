// RUN: %target-swift-ide-test -print-module -module-to-print=FriendFunction -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK:       struct A {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func memberInA(_ x: Int32)
// CHECK-NEXT:  }
// CHECK:       struct B {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func memberInB()
// CHECK-NEXT:  }
// CHECK:       struct C {
// CHECK-NEXT:    init()
// CHECK-NEXT:    func memberInB()
// CHECK-NEXT:    func memberInC()
// CHECK-NEXT:  }

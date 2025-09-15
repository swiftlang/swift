// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_CheckedContinuation
// RUN: %target-codesign %t/reflect_CheckedContinuation

// RUN: %target-run %target-swift-reflection-test %t/reflect_CheckedContinuation | %FileCheck %s --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// UNSUPPORTED: back_deployment_runtime

import SwiftReflectionTest

struct MyValue {
    let u: UInt
}

struct MyError: Error {
    let i: Int
}

@available(SwiftStdlib 5.1, *)
class MyClass {
    let cont: CheckedContinuation<MyValue, any Error>

    init(cont: CheckedContinuation<MyValue, any Error>) {
        self.cont = cont
    }
}

if #available(SwiftStdlib 5.1, *) {
  _ = try await withCheckedThrowingContinuation { checkedContinuation in
    let myClass = MyClass(cont: checkedContinuation)
    reflect(object: myClass)
    checkedContinuation.resume(returning: MyValue(u: 1))
  }
}

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (class reflect_CheckedContinuation.MyClass)

// CHECK-64: Type info:
// CHECK-64-NEXT: (class_instance size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field name=cont offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=canary offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=native)))))

// TODO: 32-bit layout

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.

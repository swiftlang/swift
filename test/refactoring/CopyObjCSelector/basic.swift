import Foundation

class MyClass: NSObject {
    @objc func simpleMethod() {
        print("simple")
    }

    @objc func methodWithParameters(param1: Int, param2: String) {
        print("params")
    }

    @objc(customSelector:with:)
    func methodWithCustomSelector(foo: Int, bar: String) {
        print("custom")
    }
}

// RUN: %refactor -source-filename %s -pos=4:16 | %FileCheck %s -check-prefix=CHECK-SIMPLE
// RUN: %refactor -source-filename %s -pos=8:16 | %FileCheck %s -check-prefix=CHECK-PARAMS
// RUN: %refactor -source-filename %s -pos=13:10 | %FileCheck %s -check-prefix=CHECK-CUSTOM

// CHECK-SIMPLE: Copy Objective-C Selector
// CHECK-PARAMS: Copy Objective-C Selector
// CHECK-CUSTOM: Copy Objective-C Selector

// REQUIRES: objc_interop

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

// RUN: %sourcekitd-test -req=objc-selector -pos=6:16 %s -- -target %target-triple -sdk %sdk %s | %FileCheck %s -check-prefix=CHECK-SIMPLE
// RUN: %sourcekitd-test -req=objc-selector -pos=10:16 %s -- -target %target-triple -sdk %sdk %s | %FileCheck %s -check-prefix=CHECK-PARAMS
// RUN: %sourcekitd-test -req=objc-selector -pos=15:10 %s -- -target %target-triple -sdk %sdk %s | %FileCheck %s -check-prefix=CHECK-CUSTOM

// CHECK-SIMPLE: simpleMethod
// CHECK-PARAMS: methodWithParametersWithParam1:param2:
// CHECK-CUSTOM: customSelector:with:

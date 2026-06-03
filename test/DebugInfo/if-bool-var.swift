// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
func doSomething() {}

// CHECK-LABEL: @"$s4main5hello5paramySb_tF"
func hello(param: Bool) {
    var mutable = param
    // CHECK: br i1 %{{[0-9]+}}, label %{{[0-9]+}}, label %{{[0-9]+}}, !dbg ![[IF_STMT:[0-9]+]]
    if mutable {
    // CHECK: ![[IF_STMT]] = !DILocation(line: [[@LINE-1]], column: 8,
        doSomething()
        mutable = false
    }
}


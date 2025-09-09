// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -O -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

public var global_c: C? = nil

@inline(never) func opaque() { print(global_c == nil ? "global is nil" : "global is not nil") }

public class C {
    init() { print("init") }
    deinit {
        print("deinit")
        global_c = self
        opaque()
        global_c = nil
        opaque()
        print("end of deinit")
    }
}

var bar: C? = C()
bar = nil
print("OK!")

// CHECK: init
// CHECK: deinit
// CHECK: global is not nil
// CHECK: global is nil
// CHECK: end of deinit
// CHECK: OK!

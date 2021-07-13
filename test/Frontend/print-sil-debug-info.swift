// Just a simple test to see whether debug info is printed to SIL
// if any of the '-g<kind>' (except '-gnone') is given.

// RUN: %target-swift-frontend -emit-sil -gnone %s | %FileCheck --check-prefix=CHECK-GNONE %s
// RUN: %target-swift-frontend -emit-sil -gline-tables-only %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -g %s | %FileCheck %s

func foo(x: Int, y: Int) -> Int {
    // CHECK: struct_extract
    // CHECK-SAME:      loc "{{.*}}.swift":{{[0-9]+}}:{{[0-9]+}}, scope {{[0-9]+}}
    // CHECK-GNONE-NOT: loc "{{.*}}.swift":{{[0-9]+}}:{{[0-9]+}}, scope {{[0-9]+}}
    x + y
}

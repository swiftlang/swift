// XFAIL: OS=windows-msvc
// REQUIRES: asan_runtime

// Default instrumentation that does not use ODR indicators
// and private aliases.
// RUN: %target-swift-frontend -emit-ir -sanitize=address %s | %FileCheck %s
// CHECK: @"[[MANGLED_GLOBAL:.+aGlobal.+]]" =
// CHECK-NOT: __odr_asan_gen_[[MANGLED_GLOBAL]]
// CHECK-NOT: @{{.+}} = private alias {{.*}}@"[[MANGLED_GLOBAL]]"

// Instrumentation with ODR indicators (implies private aliases)
// RUN: %target-swift-frontend -emit-ir -sanitize=address \
// RUN:   -sanitize-address-use-odr-indicator %s | \
// RUN:   %FileCheck -check-prefix=CHECK-ODR %s
// CHECK-ODR: @"[[MANGLED_GLOBAL:.+aGlobal.+]]" =
// CHECK-ODR: __odr_asan_gen_[[MANGLED_GLOBAL]]
// CHECK-ODR: @{{.+}} = private alias {{.*}}@"[[MANGLED_GLOBAL]]"

let aGlobal:Int = 128;

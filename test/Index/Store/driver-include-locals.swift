// This test just verifies that the flag is passed from the driver to the
// frontend. More thorough testing of the frontend behavior is in
// test/Index/local.swift.
//
// Verify the driver sets the flag for the frontend jobs:
//
// RUN: %target-swiftc_driver -driver-print-jobs -emit-object -index-ignore-system-modules -index-store-path %t/index_store -o %t/file.o %s | %FileCheck -check-prefix=JOBS %s
// RUN: %target-swiftc_driver -driver-print-jobs -emit-object -index-ignore-system-modules -index-include-locals -index-store-path %t/index_store -o %t/file.o %s | %FileCheck -check-prefix=LOCAL-JOBS %s
// JOBS-NOT: {{.*}}swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend {{.*}}-index-store-path {{.*}}/index_store{{"?}} -index-include-locals
// LOCAL-JOBS: {{.*}}swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend {{.*}}-index-store-path {{.*}}/index_store{{"?}} -index-include-locals
//
// Verify the frontend actually generates local index data:
//
// RUN: rm -rf %t
// RUN: %target-swiftc_driver -emit-object -index-ignore-system-modules -index-store-path %t/idx-no-locals -o %t/file.o %s
// RUN: %target-swiftc_driver -emit-object -index-ignore-system-modules -index-include-locals -index-store-path %t/idx-include-locals -o %t/file.o %s
// RUN: c-index-test core -print-record %t/idx-no-locals | %FileCheck -check-prefix=CHECK %s
// RUN: c-index-test core -print-record %t/idx-include-locals | %FileCheck -check-prefix=LOCAL %s

func foo(a: Int, b: Int) {
    let x = a + b
    // LOCAL: [[@LINE-1]]:9 | variable(local)/Swift | [[x_USR:.*]] | Def,RelChild | rel: 1
    // CHECK-NOT: [[@LINE-2]]:9 | variable(local)/Swift | {{.*}} | Def,RelChild | rel: 1
}

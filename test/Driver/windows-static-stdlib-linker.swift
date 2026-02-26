// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-msvc -static-stdlib %s 2>&1 | %FileCheck %s

// CHECK: clang{{(\.exe)?"? }}
// CHECK-DAG: -Xlinker /IGNORE:4217

// RUN: %swift_driver -emit-module -driver-print-jobs %s 2>&1 > %t.simple.txt
// RUN: FileCheck %s < %t.simple.txt
// RUN: FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: %swift_driver -driver-print-jobs -emit-module %s -sdk %S/../Inputs/clang-importer-sdk -Xfrontend -foo -Xfrontend -bar -o sdk.out 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swift_driver -driver-print-jobs -c -emit-module %s -o sdk.foo.out 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix TWO-OUTPUTS %s < %t.complex.txt

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: swift{{ }}
// CHECK: -emit-module
// CHECK: -o {{[^ ]+}}


// SIMPLE: swift
// SIMPLE: -emit-module
// SIMPLE: -o main.swiftmodule


// COMPLEX: swift
// COMPLEX: -emit-module
// COMPLEX-DAG: -sdk {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -foo -bar
// COMPLEX: -o sdk.out


// TWO-OUTPUTS: swift
// TWO-OUTPUTS: -emit-module
// TWO-OUTPUTS: -o main.swiftmodule

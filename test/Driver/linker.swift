// RUN: %swift_driver -driver-print-jobs -arch x86_64 %s 2>&1 > %t.simple.txt
// RUN: FileCheck %s < %t.simple.txt
// RUN: FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: %swift_driver -driver-print-jobs -arch x86_64 %s -sdk %clang-importer-sdk -lfoo -framework bar -Lbaz -Fgarply -Xlinker -undefined -Xlinker dynamic_lookup -o sdk.out 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// REQUIRES: X86

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: ld{{ }}
// CHECK-DAG: [[OBJECTFILE]]
// CHECK-DAG: -L [[STDLIB_PATH:[^ ]*/lib/swift/macosx]]
// CHECK-DAG: -rpath [[STDLIB_PATH]]
// CHECK-DAG: -lSystem
// CHECK-DAG: -arch x86_64
// CHECK-DAG: -macosx_version_min 10.8.0
// CHECK: -o {{[^ ]+}}


// SIMPLE: ld
// SIMPLE-NOT: -syslibroot
// SIMPLE: -o a.out


// COMPLEX: ld
// COMPLEX-DAG: -syslibroot {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -lfoo
// COMPLEX-DAG: -framework bar
// COMPLEX-DAG: -L baz
// COMPLEX-DAG: -F garply
// COMPLEX-DAG: -undefined dynamic_lookup
// COMPLEX: -o sdk.out

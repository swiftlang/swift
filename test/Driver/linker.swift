// RUN: %swift_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s 2>&1 > %t.simple.txt
// RUN: FileCheck %s < %t.simple.txt
// RUN: FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: %swift_driver -driver-print-jobs -emit-library -target x86_64-apple-macosx10.8 %s -sdk %S/../Inputs/clang-importer-sdk -lfoo -framework bar -Lbaz -Fgarply -Xlinker -undefined -Xlinker dynamic_lookup -o sdk.out 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swift_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -g %s | FileCheck -check-prefix DEBUG %s

// REQUIRES: X86

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: bin/ld{{ }}
// CHECK-DAG: [[OBJECTFILE]]
// CHECK-DAG: -L [[STDLIB_PATH:[^ ]*/lib/swift/macosx]]
// CHECK-DAG: -rpath [[STDLIB_PATH]]
// CHECK-DAG: -lSystem
// CHECK-DAG: -arch x86_64
// CHECK: -o {{[^ ]+}}


// SIMPLE: bin/ld{{ }}
// SIMPLE-NOT: -syslibroot
// SIMPLE-DAG: -macosx_version_min 10.{{[0-9]+}}.{{[0-9]+}}
// SIMPLE-NOT: -syslibroot
// SIMPLE: -o linker


// COMPLEX: bin/ld{{ }}
// COMPLEX-DAG: -dylib
// COMPLEX-DAG: -syslibroot {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -lfoo
// COMPLEX-DAG: -framework bar
// COMPLEX-DAG: -L baz
// COMPLEX-DAG: -F garply
// COMPLEX-DAG: -undefined dynamic_lookup
// COMPLEX-DAG: -macosx_version_min 10.8.0
// COMPLEX: -o sdk.out


// DEBUG: bin/swift
// DEBUG-NEXT: bin/swift
// DEBUG-NEXT: bin/ld{{ }}
// DEBUG: -sectalign __SWIFT __ast 4
// DEBUG: -sectcreate __SWIFT __ast {{.*}}/{{[^/]+}}.swiftmodule
// DEBUG: -o linker

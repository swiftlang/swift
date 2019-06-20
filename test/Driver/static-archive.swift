// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -module-name ARCHIVER -static 2>&1 > %t.macos.txt

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: {{(bin/)?}}libtool{{"? }} -static
// CHECK-DAG: [[OBJECTFILE]]
// CHECK: -o {{[^ ]+}}

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -emit-library %s -module-name ARCHIVER -static 2>&1 > %t.linux.txt

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: {{(bin/)?}}{{(llvm-)?}}ar{{"? }} crs
// CHECK-NEXT: {{[^ ]+}}

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-msvc -emit-library %s -module-name ARCHIVER -static 2>&1 > %t.windows.txt

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: lib -link
// CHECK-DAG: [[OBJECTFILE]]
// CHECK: /OUT:{{[^ ]+}}

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -module-name ARCHIVER -static | %FileCheck -check-prefix INFERRED_NAME_DARWIN %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -emit-library %s -module-name ARCHIVER -static | %FileCheck -check-prefix INFERRED_NAME_LINUX %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-msvc -emit-library %s -module-name ARCHIVER -static | %FileCheck -check-prefix INFERRED_NAME_WINDOWS %s

// INFERRED_NAME_DARWIN: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// INFERRED_NAME_DARWIN: -module-name ARCHIVER
// INFERRED_NAME_DARWIN: libtool -static
// INFERRED_NAME_DARWIN:  -o libARCHIVER.a
// INFERRED_NAME_LINUX:   libARCHIVER.a
// INFERRED_NAME_WINDOWS: -lib
// INFERRED_NAME_WINDOWS: libARCHIVER.lib

// RUN: not %swiftc_driver -driver-print-jobs -module-name ARCHIVER  %s -emit-executable -static 2>&1 | %FileCheck -check-prefix ERROR %s

// ERROR: error: -static may not be used with -emit-executable

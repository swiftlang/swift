// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s 2>&1 > %t.simple.txt
// RUN: FileCheck %s < %t.simple.txt
// RUN: FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios7.1 %s 2>&1 > %t.simple.txt
// RUN: FileCheck -check-prefix IOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -Ffoo -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: FileCheck -check-prefix LINUX %s < %t.linux.txt

// RUN: %swiftc_driver -driver-print-jobs -emit-library -target x86_64-apple-macosx10.9.1 %s -sdk %S/../Inputs/clang-importer-sdk -lfoo -framework bar -Lbaz -Fgarply -Xlinker -undefined -Xlinker dynamic_lookup -o sdk.out 2>&1 > %t.complex.txt
// RUN: FileCheck %s < %t.complex.txt
// RUN: FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -g %s | FileCheck -check-prefix DEBUG %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -g %s | FileCheck -check-prefix DEBUG_LINUX %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 %s | FileCheck -check-prefix NO_ARCLITE %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios8.0 %s | FileCheck -check-prefix NO_ARCLITE %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -module-name LINKER | FileCheck -check-prefix INFERRED_NAME %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -o libLINKER.dylib | FileCheck -check-prefix INFERRED_NAME %s

// REQUIRES: X86

// FIXME: Need to set up a sysroot for osx so the DEBUG checks work on linux
// rdar://problem/19692770
// XFAIL: linux

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: bin/ld{{"? }}
// CHECK-DAG: [[OBJECTFILE]]
// CHECK-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift/macosx]]
// CHECK-DAG: -rpath [[STDLIB_PATH]]
// CHECK-DAG: -lSystem
// CHECK-DAG: -arch x86_64
// CHECK-DAG: -force_load {{[^ ]+/lib/arc/libarclite_macosx.a}}
// CHECK: -o {{[^ ]+}}


// SIMPLE: bin/ld{{"? }}
// SIMPLE-NOT: -syslibroot
// SIMPLE-DAG: -macosx_version_min 10.{{[0-9]+}}.{{[0-9]+}}
// SIMPLE-NOT: -syslibroot
// SIMPLE: -o linker


// IOS_SIMPLE: swift
// IOS_SIMPLE: -o [[OBJECTFILE:.*]]

// IOS_SIMPLE: bin/ld{{"? }}
// IOS_SIMPLE-DAG: [[OBJECTFILE]]
// IOS_SIMPLE-DAG: -L {{[^ ]+/lib/swift/iphonesimulator}}
// IOS_SIMPLE-DAG: -lSystem
// IOS_SIMPLE-DAG: -arch x86_64
// IOS_SIMPLE-DAG: -ios_simulator_version_min 7.1.{{[0-9]+}}
// IOS_SIMPLE: -o linker

// LINUX: swift
// LINUX: -o [[OBJECTFILE:.*]]

// LINUX: clang++{{"? }}
// LINUX-DAG: [[OBJECTFILE]]
// LINUX-DAG: -lswiftCore
// LINUX-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
// LINUX-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-DAG: -Xlinker -T /{{[^ ]+}}/linux/x86_64/swift.ld
// LINUX-DAG: -F foo
// LINUX-DAG: -framework bar
// LINUX-DAG: -L baz
// LINUX-DAG: -lboo
// LINUX-DAG: -Xlinker -undefined
// LINUX: -o linker

// COMPLEX: bin/ld{{"? }}
// COMPLEX-DAG: -dylib
// COMPLEX-DAG: -syslibroot {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -lfoo
// COMPLEX-DAG: -framework bar
// COMPLEX-DAG: -L baz
// COMPLEX-DAG: -F garply
// COMPLEX-DAG: -undefined dynamic_lookup
// COMPLEX-DAG: -macosx_version_min 10.9.1
// COMPLEX: -o sdk.out


// DEBUG: bin/swift
// DEBUG-NEXT: bin/swift
// DEBUG-NEXT: bin/ld{{"? }}
// DEBUG: -add_ast_path {{.*}}/{{[^/]+}}.swiftmodule
// DEBUG: -o linker
// DEBUG-NEXT: bin/dsymutil
// DEBUG: linker
// DEBUG: -o linker.dSYM

// DEBUG_LINUX: bin/swift
// DEBUG_LINUX-NEXT: bin/swift
// DEBUG_LINUX-NEXT: bin/clang++{{"? }}
// DEBUG_LINUX: -o linker
// DEBUG_LINUX-NOT: dsymutil

// NO_ARCLITE: bin/ld{{"? }}
// NO_ARCLITE-NOT: arclite
// NO_ARCLITE: -o {{[^ ]+}}


// INFERRED_NAME: bin/swift
// INFERRED_NAME: -module-name LINKER
// INFERRED_NAME: bin/ld{{"? }}
// INFERRED_NAME: -o libLINKER.dylib

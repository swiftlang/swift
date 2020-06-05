// RUN: %target-swiftc_driver -driver-print-jobs %S/../Inputs/empty.swift -lto=llvm | %FileCheck %s --check-prefix=CHECK-%target-os --check-prefix=CHECK
// CHECK: swift{{(c\.exe")?}} -frontend -emit-bc
// CHECK-macosx-NEXT: bin/ld {{.+}} -lto_library {{.+}}/lib/libLTO.dylib
// CHECK-windows-msvc-NEXT: clang.exe" {{.+}} -fuse-ld=lld -flto=thin
// CHECK-linux-gnu-NEXT: bin/clang {{.+}} -flto=thin

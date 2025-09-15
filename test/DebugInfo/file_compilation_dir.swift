// UNSUPPORTED: OS=windows-msvc
// RUN: %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir /path/to \
// RUN:   %s -o - -emit-ir | %FileCheck --check-prefix=CHECK-ABS %s
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: cd %t
// RUN: cp %s .
// Output paths differ in the new driver, so force SWIFT_USE_OLD_DRIVER for now.
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir /path/to \
// RUN:   file_compilation_dir.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL %s
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir . \
// RUN:   file_compilation_dir.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL-CWD %s

func foo() {}

// CHECK-ABS: !DIFile(filename: "{{.*}}/file_compilation_dir.swift", directory: "/path/to")
// CHECK-REL: !DIFile(filename: "file_compilation_dir.swift", directory: "/path/to")
// CHECK-REL-CWD: !DIFile(filename: "file_compilation_dir.swift", directory: ".")

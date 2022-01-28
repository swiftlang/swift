// REQUIRES: OS=windows-msvc
// RUN: %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir Z:\path\to \
// RUN:   %s -o - -emit-ir | %FileCheck --check-prefix=CHECK-ABS %s
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: cd %t
// RUN: xcopy %s .
// RUN: %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir Z:\path\to \
// RUN:   file_compilation_dir_windows.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL %s
// RUN: %target-swiftc_driver -g \
// RUN:   -c -file-compilation-dir . \
// RUN:   file_compilation_dir_windows.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL-CWD %s

func foo() {}

// CHECK-ABS: !DIFile(filename: "{{[a-zA-Z]:\\\\.*\\\\}}file_compilation_dir_windows.swift", directory: "Z:\\path\\to")
// CHECK-REL: !DIFile(filename: "file_compilation_dir_windows.swift", directory: "Z:\\path\\to")
// CHECK-REL-CWD: !DIFile(filename: "file_compilation_dir_windows.swift", directory: ".")

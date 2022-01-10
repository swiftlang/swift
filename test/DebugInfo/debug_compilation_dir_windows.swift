// REQUIRES: OS=windows-msvc
// RUN: %target-swiftc_driver -g \
// RUN:   -c -debug-compilation-dir Z:\path\to \
// RUN:   %s -o - -emit-ir | %FileCheck --check-prefix=CHECK-ABS %s
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: cd %t
// RUN: cp %s .
// RUN: %target-swiftc_driver -g \
// RUN:   -c -debug-compilation-dir Z:\path\to \
// RUN:   debug_compilation_dir.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL %s
// RUN: %target-swiftc_driver -g \
// RUN:   -c -debug-compilation-dir . \
// RUN:   debug_compilation_dir.swift -o - -emit-ir | %FileCheck --check-prefix=CHECK-REL-CWD %s

func foo() {}

// CHECK-ABS: !DIFile(filename: "{{[a-zA-Z]:\\.*\\}}debug_compilation_dir.swift", directory: "Z:\path\to")
// CHECK-REL: !DIFile(filename: "debug_compilation_dir.swift", directory: "Z:\path\to")
// CHECK-REL-CWD: !DIFile(filename: "debug_compilation_dir.swift", directory: ".")
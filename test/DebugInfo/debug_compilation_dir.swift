// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: cd %t
// RUN: cp %s .
// RUN: %target-swift-frontend -g \
// RUN:   -wmo -c -debug-compilation-dir /var/empty \
// RUN:   debug_compilation_dir.swift -o - -emit-ir | %FileCheck %s

func foo() {}

// CHECK: !DIFile(filename: "debug_compilation_dir.swift", directory: "/var/empty")

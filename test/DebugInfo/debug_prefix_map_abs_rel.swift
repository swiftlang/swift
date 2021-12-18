// RUN: %empty-directory(%t)
// RUN: mkdir -p %t
// RUN: cd %t
// RUN: cp %s .
// RUN: %target-swiftc_driver -g -debug-prefix-map %t=. \
// RUN:   debug_prefix_map_abs_rel.swift \
// RUN:   -emit-ir -o - | %FileCheck %s

public func f() {}

// CHECK-NOT: debug_prefix_map_abs_rel.swift
// CHECK: !DIFile(filename: "debug_prefix_map_abs_rel.swift", directory: ".")
// CHECK-NOT: debug_prefix_map_abs_rel.swift

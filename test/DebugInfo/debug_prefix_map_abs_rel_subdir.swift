// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/SubdirectoryWithAVeryUniqueName
// RUN: cd %t/SubdirectoryWithAVeryUniqueName
// RUN: cp %s .
// RUN: %target-swiftc_driver -g -debug-prefix-map %t=. \
// RUN:   %t/SubdirectoryWithAVeryUniqueName/debug_prefix_map_abs_rel_subdir.swift \
// RUN:   -emit-ir -o - | %FileCheck %s

public func f() {}

// CHECK-NOT: debug_prefix_map_abs_rel_subdir.swift
// CHECK: !DIFile(filename: "debug_prefix_map_abs_rel_subdir.swift", directory: ".{{/|\\\\}}SubdirectoryWithAVeryUniqueName")
// CHECK-NOT: debug_prefix_map_abs_rel_subdir.swift

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN: %target-swift-frontend -c -I %t -primary-file %s -g -parse-as-library -emit-module -emit-object -module-cache-path "./mc" -file-compilation-dir "." -debug-prefix-map "$(pwd)=." -o %t/test.o
// RUN: %llvm-dwarfdump %t/test.o | %FileCheck %s

// CHECK: DW_TAG_compile_unit

// CHECK: DW_TAG_compile_unit
// CHECK-NOT: NULL
// CHECK: DW_AT_comp_dir	(".")
// CHECK-NOT: NULL

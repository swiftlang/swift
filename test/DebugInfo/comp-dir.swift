// RUN: %empty-directory(%t)
// RUN %target-swift-frontend -emit-module-path %t/Globals.swiftmodule %S/Globals.swift
// RUN %target-swift-frontend -c -I %t -primary-file %s -g -parse-as-library -emit-module -emit-object -module-cache-path "./mc" -file-compilation-dir "." -debug-prefix-map "$(pwd)=." -o %t/test.o
// RUN %llvm-dwarfdump %t/test.o | %FileCheck %s

// RUN: %target-swift-frontend \
// RUN:   -emit-pch %S/Inputs/BridgingHeader.h -o %t.pch

// RUN: %target-swift-frontend -c -g -primary-file %s -file-compilation-dir "."  -import-objc-header %t.pch -o %t/test2.o
// RUN: %llvm-dwarfdump %t/test2.o | %FileCheck %s --check-prefix=PCH


// CHECK: DW_TAG_compile_unit

// CHECK: DW_TAG_compile_unit
// CHECK-NOT: NULL
// CHECK: DW_AT_comp_dir	(".")
// CHECK-NOT: NULL

// PCH: DW_TAG_compile_unit

// PCH: DW_TAG_compile_unit
// PCH:  DW_AT_name	("BridgingHeader.h")
// PCH-NOT: DW_AT_comp_dir	(".")

// PCH: DW_TAG_compile_unit

var P : Point? = nil
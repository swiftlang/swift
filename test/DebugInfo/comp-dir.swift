// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -primary-file %s -g -parse-as-library -emit-module -emit-object -module-cache-path "./mc" -file-compilation-dir "." -debug-prefix-map "$(pwd)=." -o %t/test.o
// RUN: %llvm-dwarfdump %t/test.o | %FileCheck %s

// CHECK: DW_TAG_compile_unit
// CHECK: DW_TAG_compile_unitaca
// CHECK-NOT: NULL
// CHECK: DW_AT_comp_dir	(".")
// CHECK-NOT: NULL

// CHECK: DW_TAG_compile_unit
// CHECK-NOT: NULL
// CHECK: DW_AT_comp_dir	(".")
// CHECK-NOT: NULL

import Foundation


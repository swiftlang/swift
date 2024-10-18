// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/BuildRoot && cd %t/BuildRoot

// Check paths are mapped
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %S=REMAPPED_SRC_ROOT -file-prefix-map %t=REMAPPED_OUT_DIR %s -o %t/file1.o -typecheck
// RUN: c-index-test core -print-unit %t/idx > %t/units.out

// Check round-trip remapping to make sure they're converted back to the local paths
// RUN: c-index-test core -print-unit %t/idx -index-store-prefix-map REMAPPED_SRC_ROOT=%S -index-store-prefix-map REMAPPED_OUT_DIR=%t >> %t/units.out

// RUN: %FileCheck %s --dump-input-filter all < %t/units.out

// CHECK: file1.o-[[UNIT_HASH:.*$]]
// CHECK: --------
// CHECK: main-path: REMAPPED_SRC_ROOT{{/|\\}}unit-one-sourcefile-remapped.swift
// CHECK: work-dir: REMAPPED_OUT_DIR{{/|\\}}BuildRoot
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}file1.o
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{BUILD_DIR|.*lib\\swift\\windows}}{{.*}}Swift.swiftmodule
// CHECK: DEPEND END

// CHECK: file1.o-[[UNIT_HASH]]
// CHECK: --------
// CHECK: main-path: SOURCE_DIR{{/|\\}}test{{/|\\}}Index{{/|\\}}Store{{/|\\}}unit-one-sourcefile-remapped.swift
// CHECK-NOT: work-dir: REMAPPED_OUT_DIR

// CHECK-NOT: file1.o-[[UNIT_HASH]]

// RUN: rm -rf %t
// RUN: mkdir -p %t/BuildRoot && cd %t/BuildRoot
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %S=REMAPPED_SRC_ROOT -file-prefix-map %t=REMAPPED_OUT_DIR %s -o %t/file1.o -typecheck
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s --dump-input-filter all

// CHECK: file1.o-1AYKXZF3HH50A
// CHECK: --------
// CHECK: main-path: REMAPPED_SRC_ROOT{{/|\\}}unit-one-sourcefile-remapped.swift
// CHECK: work-dir: REMAPPED_OUT_DIR{{/|\\}}BuildRoot
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}file1.o
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{BUILD_DIR|.*lib\\swift\\windows}}{{.*}}Swift.swiftmodule
// CHECK: DEPEND END


// Check round-trip remapping to make sure they're converted back to the local paths.
// RUN: c-index-test core -print-unit %t/idx -index-store-prefix-map REMAPPED_SRC_ROOT=%S -index-store-prefix-map  REMAPPED_OUT_DIR=%t | %FileCheck %s -check-prefix=ROUNDTRIP --dump-input-filter all

// ROUNDTRIP: file1.o-1AYKXZF3HH50A
// ROUNDTRIP: --------
// ROUNDTRIP: main-path: SOURCE_DIR{{/|\\}}test{{/|\\}}Index{{/|\\}}Store{{/|\\}}unit-one-sourcefile-remapped.swift
// ROUNDTRIP-NOT: work-dir: REMAPPED_OUT_DIR

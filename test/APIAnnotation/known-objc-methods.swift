// RUN: %swift-ide-test -generate-api-annotation -o %t
// RUN: llvm-bcanalyzer %t | FileCheck %s

// CHECK: Block ID #0 (BLOCKINFO_BLOCK)
// CHECK: Block ID #[[SIDE_CAR:[0-9]+]] (SIDE_CAR_BLOCK)

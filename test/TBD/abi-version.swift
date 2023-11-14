// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)

// This test ensures that we see the same Swift ABI Version flag in the LLVM IR
// and in the TBD.

// 1. Emit IR and a TBD for this file

// RUN: %target-swift-frontend -emit-ir -o %t/test.ll %s -emit-tbd-path %t/test.tbd -tbd-install_name test

// 2. Concatenate them and FileCheck them both in the same file, so we can capture
//    the ABI version in a variable.

// RUN: cat %t/test.ll %t/test.tbd | %FileCheck %s

// 3. Look in the IR for the Swift Version flag

// CHECK: !"Swift Version", i32 [[VERSION:[0-9]+]]

// 4. Look in the TBD for the same version listed

// CHECK: "abi": [[VERSION]]

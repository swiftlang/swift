// Make sure that we can deserialize CoreGraphics.
// RUN: %target-sil-opt -verify %platform-module-dir/CoreGraphics.swiftmodule > /dev/null
// RUN: llvm-bcanalyzer %platform-module-dir/CoreGraphics.swiftmodule | FileCheck %s

// REQUIRES: sdk

// CHECK-NOT: Unknown

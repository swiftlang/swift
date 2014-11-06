// Make sure that we can deserialize CoreGraphics.
// RUN: %sil-opt -verify %platform-module-dir/CoreGraphics.swiftmodule -sdk %sdk -target %target-triple > /dev/null
// RUN: llvm-bcanalyzer %platform-module-dir/CoreGraphics.swiftmodule | FileCheck %s
// REQUIRES: sdk

// CHECK-NOT: Unknown

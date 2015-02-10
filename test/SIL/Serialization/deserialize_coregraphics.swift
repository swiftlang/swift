// Make sure that we can deserialize CoreGraphics.
// RUN: %target-sil-opt -verify %platform-sdk-overlay-dir/CoreGraphics.swiftmodule > /dev/null
// RUN: llvm-bcanalyzer %platform-sdk-overlay-dir/CoreGraphics.swiftmodule | FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: Unknown

// Make sure that we can deserialize CoreGraphics.
// RUN: %target-sil-opt %platform-sdk-overlay-dir/CoreGraphics.swiftmodule > /dev/null
// RUN: llvm-bcanalyzer %platform-sdk-overlay-dir/CoreGraphics.swiftmodule | FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: rdar://23667534

// CHECK-NOT: Unknown

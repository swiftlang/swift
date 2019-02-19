// Make sure that we can deserialize CoreGraphics.
// RUN: %target-sil-opt %platform-sdk-overlay-dir/CoreGraphics.swiftmodule/%target-swiftmodule-name -module-name CoreGraphics > /dev/null
// RUN: llvm-bcanalyzer %platform-sdk-overlay-dir/CoreGraphics.swiftmodule/%target-swiftmodule-name | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: Unknown

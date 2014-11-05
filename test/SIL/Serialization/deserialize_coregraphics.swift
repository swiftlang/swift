// Make sure that we can deserialize CoreGraphics.
// RUN: %sil-opt -verify %libdir/swift/macosx/CoreGraphics.swiftmodule -module-name CoreGraphics -sdk %sdk -target %target-triple > /dev/null
// RUN: llvm-bcanalyzer %libdir/swift/macosx/CoreGraphics.swiftmodule | FileCheck %s
// REQUIRES: sdk
// REQUIRES: OS=macosx

// CHECK-NOT: Unknown

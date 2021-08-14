// RUN: %target-swift-frontend -emit-ir %s -target x86_64-apple-macos10.15 | %FileCheck %s --check-prefix=CHECK10
// RUN: %target-swift-frontend -emit-ir %s -target x86_64-apple-macos11 | %FileCheck %s --check-prefix=CHECK11

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// UNSUPPORTED: use_os_stdlib

@available(macOS 11, *)
public struct Float16Wrapper {
  @available(macOS, unavailable)
  var x: Float16
}

// CHECK10-LABEL: @"$ss7Float16VMn" = extern_weak global %swift.type_descriptor
// CHECK11-LABEL: @"$ss7Float16VMn" = external global %swift.type_descriptor

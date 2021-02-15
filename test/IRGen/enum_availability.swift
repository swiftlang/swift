// RUN: %target-swift-emit-ir %s | %FileCheck %s

// REQUIRES: OS=macosx


// CHECK: @"$ss7Float16VMn" = extern_weak global %swift.type_descriptor

internal enum Payload {
  case float(Float)

  @available(iOS 14.0, *)
  @available(macOS, unavailable)
  case half(Float16)

  init?() {
    return nil
  }
}

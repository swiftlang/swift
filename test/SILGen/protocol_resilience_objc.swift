// RUN: %target-swift-frontend -emit-silgen -disable-objc-attr-requires-foundation-module -enable-resilience %s | %FileCheck %s --check-prefix=CHECK
// REQUIRES: objc_interop

// @objc protocols don't need default witness tables
@objc public protocol ObjCProtocol {
  func f()
  func g()
}

// CHECK-NOT: sil_default_witness_table ObjCProtocol

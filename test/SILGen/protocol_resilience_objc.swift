// RUN: %target-swift-emit-silgen -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-sil-ownership -enable-resilience %s | %FileCheck %s --check-prefix=CHECK

// @objc protocols don't need default witness tables
@objc public protocol ObjCProtocol {
  func f()
  func g()
}

// CHECK-NOT: sil_default_witness_table ObjCProtocol

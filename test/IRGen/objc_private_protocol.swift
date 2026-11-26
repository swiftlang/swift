// RUN: %target-swift-frontend -emit-ir -disable-objc-attr-requires-foundation-module -enable-objc-interop -conditional-runtime-records %s | %FileCheck %s

// REQUIRES: objc_interop

// The field descriptor's conditional liveness entry must not reference a
// Swift protocol descriptor for @objc protocols, which use ObjC protocol
// records and never define a Swift descriptor.

// CHECK-NOT: Global is external
// CHECK: @_PROTOCOL_

@objc private protocol P {}
@objc fileprivate protocol Q {}

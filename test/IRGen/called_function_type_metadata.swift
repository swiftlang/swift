// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature CalledAttribute -disable-concrete-type-metadata-mangled-name-accessors | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

// A function type records its execution semantics in its extended flags
// (0x20 for at-most-once, 0x40 for exactly-once), along with the invertible
// protocols that it suppresses in the high bits: an at-most-once function type
// suppresses Copyable (0x10000), and an exactly-once function type also
// suppresses Deinitable (0x50000).

public func calledMetatypes() -> (UnsafeRawPointer, UnsafeRawPointer) {
  (unsafeBitCast((@called(atMostOnce) () -> Void).self,
                 to: UnsafeRawPointer.self),
   unsafeBitCast((@called(exactlyOnce) () -> Void).self,
                 to: UnsafeRawPointer.self))
}

// CHECK-LABEL: define {{.*}} @"$syyXOoMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata({{.*}}, i32 65568, ptr null)

// CHECK-LABEL: define {{.*}} @"$syyXOMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata({{.*}}, i32 327744, ptr null)

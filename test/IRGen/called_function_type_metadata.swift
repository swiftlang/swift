// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature CalledAttribute -enable-experimental-feature NondeinitableTypes -disable-concrete-type-metadata-mangled-name-accessors | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: swift_feature_NondeinitableTypes

// A `@called` function type records its execution semantics as the invertible
// protocols that it suppresses, in the high bits of its extended flags:
// `@called(atMostOnce)` suppresses Copyable (0x10000), and
// `@called(exactlyOnce)` also suppresses Deinitable (0x50000).

public func takeMetatype<T: ~Copyable & ~Deinitable>(_: T.Type) {}

public func calledMetatypes() {
  takeMetatype((@called(atMostOnce) () -> Void).self)
  takeMetatype((@called(exactlyOnce) () -> Void).self)
}

// CHECK-LABEL: define {{.*}} @"$syyXOoMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata({{.*}}, i32 65536, ptr null)

// CHECK-LABEL: define {{.*}} @"$syyXOMa"
// CHECK: call ptr @swift_getExtendedFunctionTypeMetadata({{.*}}, i32 327680, ptr null)

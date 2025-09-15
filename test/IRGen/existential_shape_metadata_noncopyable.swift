// RUN: %target-swift-frontend \
// RUN:    -emit-ir %s -swift-version 5 \
// RUN:   -disable-availability-checking \
// RUN:     -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -module-name existential_shape_metadata | %IRGenFileCheck %s

// REQUIRES: swift_feature_SuppressedAssociatedTypes

// NOTE: Once noncopyable generics are enabled by default, merge this back into existential_shape_metadata.swift

// CHECK: @"$sl26existential_shape_metadata3QNC_px1ARts_XPXGMq" = linkonce_odr

public protocol QNC<A>: ~Copyable {
  associatedtype A: ~Copyable
}

public struct NCStruct: ~Copyable { }


public func testNoncopyableConcrete() -> any  ~Copyable.Type {
  return (any QNC<NCStruct>).self
}

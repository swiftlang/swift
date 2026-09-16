// Reproducer for rdar://182139366
//
// Ensure that layout string value witnesses are ignored in embedded mode

// RUN: %target-swift-frontend -module-name foo -emit-ir %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature LayoutStringValueWitnesses -enable-layout-string-value-witnesses | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_LayoutStringValueWitnesses

// Referencing `Any` as a first-class type forces emission of its type
// metadata, which in embedded mode emits its value witness table.
public func anyMetatype() -> Any.Type {
  return Any.self
}

// CHECK: define {{.*}}anyMetatype

// RUN: %target-swift-frontend -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-layout-string-value-witnesses -enable-type-layout -emit-ir -import-objc-header %S/Inputs/union_type.h %s

// REQUIRES: swift_feature_LayoutStringValueWitnesses

struct UnionWrapper {
    let x: TestUnion
}

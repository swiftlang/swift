// RUN: %target-swift-frontend -enable-experimental-feature LayoutStringValueWitnesses -enable-type-layout -emit-ir -import-objc-header %S/Inputs/union_type.h %s

struct UnionWrapper {
    let x: TestUnion
}

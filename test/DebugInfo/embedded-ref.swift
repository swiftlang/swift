// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -parse-as-library -wmo -module-name main -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Lifetimes

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Holder", {{.*}}elements: ![[HOLDER_ELTS:[0-9]+]], {{.*}}identifier: "$e4main6HolderVD")
// CHECK-DAG: ![[HOLDER_ELTS]] = !{![[REF_MEMBER:[0-9]+]]}
// CHECK-DAG: ![[REF_MEMBER]] = !DIDerivedType(tag: DW_TAG_member, name: "ref", {{.*}}size: 64)
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Ref", {{.*}}elements: ![[REF_ELTS:[0-9]+]], {{.*}}identifier: "$es3RefVyxGD")
// CHECK-DAG: ![[REF_ELTS]] = !{![[BUILTIN_MEMBER:[0-9]+]]}
// CHECK-DAG: ![[BUILTIN_MEMBER]] = !DIDerivedType(tag: DW_TAG_member, name: "builtin", {{.*}}baseType: ![[BORROW:[0-9]+]])
// CHECK-DAG: ![[BORROW]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Builtin.Borrow", {{.*}}identifier: "$exBWD")

struct Holder: ~Escapable {
  let ref: Ref<Int>

  @_lifetime(borrow value)
  init(_ value: borrowing Int) {
    ref = Ref(value)
  }
}

public func run() -> Int {
  let x = 1
  let h = Holder(x)
  return h.ref.value
}

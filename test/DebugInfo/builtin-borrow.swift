// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -module-name main \
// RUN:   -enable-experimental-feature BuiltinModule \
// RUN:   -enable-experimental-feature Lifetimes -o - | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes

// Builtin.Borrow is emitted as an opaque type identified by its mangled name,
// sized only when the referent's layout is known.

import Builtin

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "MyRef", {{.*}}elements: ![[MYREF_ELTS:[0-9]+]], {{.*}}identifier: "$s4main5MyRefVyxGD")
// CHECK-DAG: ![[MYREF_ELTS]] = !{![[BUILTIN_MEMBER:[0-9]+]]}
// CHECK-DAG: ![[BUILTIN_MEMBER]] = !DIDerivedType(tag: DW_TAG_member, name: "builtin", {{.*}}baseType: ![[GENERIC_BORROW:[0-9]+]])
// CHECK-DAG: ![[GENERIC_BORROW]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Builtin.Borrow", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, runtimeLang: DW_LANG_Swift, identifier: "$sxBWD")
struct MyRef<Value>: ~Escapable {
  let builtin: Builtin.Borrow<Value>

  @_lifetime(borrow value)
  init(_ value: borrowing Value) {
    builtin = Builtin.makeBorrow(value)
  }
}

// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "inline", {{.*}}baseType: ![[INLINE_BORROW:[0-9]+]], size: {{32|64}}
// CHECK-DAG: ![[INLINE_BORROW]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Builtin.Borrow", {{.*}}size: {{32|64}}, {{.*}}identifier: "$sSiBWD")
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "indirect", {{.*}}baseType: ![[POINTER_BORROW:[0-9]+]], size: {{32|64}}
// CHECK-DAG: ![[POINTER_BORROW]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Builtin.Borrow", {{.*}}size: {{32|64}}, {{.*}}identifier: "$sypBWD")
struct Holder: ~Escapable {
  let ref: MyRef<Int>
  let inline: Builtin.Borrow<Int>
  let indirect: Builtin.Borrow<Any>

  @_lifetime(borrow value, borrow any)
  init(_ value: borrowing Int, _ any: borrowing Any) {
    ref = MyRef(value)
    inline = Builtin.makeBorrow(value)
    indirect = Builtin.makeBorrow(any)
  }
}

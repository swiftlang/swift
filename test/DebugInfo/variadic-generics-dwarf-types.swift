// RUN: %target-swift-frontend -emit-ir %s -gdwarf-types -o - \
// RUN:    -parse-as-library -module-name a -target %target-swift-5.9-abi-triple | %FileCheck %s

// Tuples containing a pack expansion have no static element layout, so with
// -gdwarf-types they are described as opaque structs, as with -g.

public struct V<each T> { public var x: (repeat each T) }

public func expansion<each T>(_ t: repeat each T) {
  let p: (repeat each T) = (repeat each t)
  _ = p
}

public func mixed<each T>(_ t: repeat each T) {
  let p: (Int, repeat each T) = (1, repeat each t)
  _ = p
}

public func nested<each T>(_ t: repeat each T) {
  let p: ((repeat each T), Int) = ((repeat each t), 1)
  _ = p
}

public func param<each T>(_ p: (repeat each T)) {
  _ = p
}

public func concreteVariadicStruct() {
  let v = V<Int, String>(x: (1, "a"))
  _ = v
}

public func genericVariadicStruct<each T>(_ t: repeat each T) {
  let v = V<repeat each T>(x: (repeat each t))
  _ = v
}

// CHECK-DAG: ![[EXP:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxxQp_tD", file: ![[#]], runtimeLang: DW_LANG_Swift, identifier: "$sxxQp_tD")
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sSi_xxQptD", file: ![[#]], runtimeLang: DW_LANG_Swift, identifier: "$sSi_xxQptD")

// Only the inner tuple is opaque.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sxxQp_t_SitD", {{.*}}elements: ![[NESTED_ELTS:[0-9]+]]
// CHECK-DAG: ![[NESTED_ELTS]] = !{![[INNER:[0-9]+]], ![[#]]}
// CHECK-DAG: ![[INNER]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[EXP]])

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "V", {{.*}}elements: ![[V_ELTS:[0-9]+]]
// CHECK-DAG: ![[V_ELTS]] = !{![[V_X:[0-9]+]]}
// CHECK-DAG: ![[V_X]] = !DIDerivedType(tag: DW_TAG_member, name: "x", {{.*}}baseType: ![[EXP]])

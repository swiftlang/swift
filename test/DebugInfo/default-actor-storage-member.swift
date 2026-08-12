// A root default actor has a fixed-size storage field laid out immediately
// after the object header and ahead of its stored properties.
// NominalTypeDecl::getStoredProperties() does not report it — forEachField()
// does — and GenReflection already emits it into the field descriptor, so DWARF
// used to be the only description of the layout that left it out. Anything that
// reconstructs the layout from DWARF alone, which is every consumer under
// embedded Swift where no reflection metadata exists, then placed the first
// stored property at the top of the instance and read a wrong value rather than
// no value.

// RUN: %target-swift-frontend %s -emit-ir -g -gdwarf-types \
// RUN:   -parse-as-library -module-name main -o - \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-ptrsize

// REQUIRES: concurrency

public actor Plain {
  public let str = "Hello"
}

// A non-actor class must not gain the field.
public class NotAnActor {
  public let str = "Hello"
}

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Plain",{{.*}}elements: ![[PLAIN_ELTS:[0-9]+]]
// CHECK-DAG: ![[PLAIN_ELTS]] = !{![[STORAGE:[0-9]+]], ![[STR:[0-9]+]]}

// The member is artificial, and its size is the whole default-actor buffer —
// NumWords_DefaultActor (12) pointer-sized words — not a single pointer. Its
// type keeps its mangled name — `Builtin.DefaultActorStorage` is deliberately
// not lowered to the "<unknown>" placeholder — because that name is how a
// debugger recovers the buffer's size. The stored property follows the buffer,
// at the buffer's size.

// CHECK-64-DAG: ![[STORAGE]] = !DIDerivedType(tag: DW_TAG_member, name: "$defaultActor",{{.*}}baseType: ![[STORAGE_TY:[0-9]+]], size: 768, flags: DIFlagArtificial)
// CHECK-64-DAG: ![[STORAGE_TY]] = !DIBasicType(name: "$sBDD", size: 768)
// CHECK-64-DAG: ![[STR]] = !DIDerivedType(tag: DW_TAG_member, name: "str",{{.*}}offset: 768)

// CHECK-32-DAG: ![[STORAGE]] = !DIDerivedType(tag: DW_TAG_member, name: "$defaultActor",{{.*}}baseType: ![[STORAGE_TY:[0-9]+]], size: 384, flags: DIFlagArtificial)
// CHECK-32-DAG: ![[STORAGE_TY]] = !DIBasicType(name: "$sBDD", size: 384)
// CHECK-32-DAG: ![[STR]] = !DIDerivedType(tag: DW_TAG_member, name: "str",{{.*}}offset: 384)

// A non-actor class keeps a one-element member list, and its member stays at
// offset 0 — which the metadata printer spells by omitting `offset:`, so the
// closing parenthesis is the assertion here.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "NotAnActor",{{.*}}elements: ![[NOT_ELTS:[0-9]+]]
// CHECK-DAG: ![[NOT_ELTS]] = !{![[NOT_STR:[0-9]+]]}
// CHECK-DAG: ![[NOT_STR]] = !DIDerivedType(tag: DW_TAG_member, name: "str", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, baseType: !{{[0-9]+}}, size: {{[0-9]+}})

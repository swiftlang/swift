// RUN: %target-swift-frontend  %s -emit-ir -g -o - | %FileCheck %s

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "A",{{.*}}elements: ![[A_ELTS:[0-9]+]], runtimeLang: DW_LANG_Swift, identifier: "$s15recursive_actor1ACyxGD")
// A root default actor's artificial storage field is laid out ahead of its
// stored properties, so it is the first element.
// CHECK: ![[A_ELTS]] = !{![[M_ACTOR:[0-9]+]], ![[M_CHILD:[0-9]+]]}
// CHECK: ![[M_ACTOR]] = !DIDerivedType(tag: DW_TAG_member, name: "$defaultActor", {{.*}}flags: DIFlagArtificial
// CHECK: ![[M_CHILD]] = !DIDerivedType(tag: DW_TAG_member, name: "children", {{.*}}baseType: ![[CONT_AB:[0-9]+]]
// CHECK: ![[CONT_AB]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}elements: ![[C_ELTS:[0-9]+]]
// CHECK: ![[C_ELTS]] = !{![[M_AB:[0-9]+]]}
// CHECK: ![[M_AB]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[AB:[0-9]+]]
// CHECK: ![[AB:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSay15recursive_actor1ACyACyxGGGD"

public actor A<Parent>: MutableA where Parent: MutableA {
  public let children: [A<A>] = []
}

public protocol MutableA: Actor {
  associatedtype Child where Child: MutableA
  var children: [Child] { get }
}

public actor B: MutableA {
  public private(set) var children: [A<B>] = []
}


// RUN: %target-swift-frontend -emit-ir %s -g -O -parse-as-library -module-name a \
// RUN:    -o - | %FileCheck %s

// A generic method that is force-inlined once per element of a parameter-pack
// expansion is inlined multiple times from a single generic specialization,
// each time with different concrete type arguments (here 'self' is 'Box<Int>'
// for one instance and 'Box<String>' for another). Those two instances must not
// be collapsed onto a single abstract DISubprogram

struct Box<Value> {
  var value: Value

  @inline(__always)
  func transform<Result>(_ body: (Value) -> Result) -> Result {
    return body(value)
  }
}

@inline(__always)
func packEngine<each T, each R>(
  _ items: (repeat Box<each T>),
  _ logics: repeat (each T) -> each R
) -> (repeat each R) {
  return (repeat (each items).transform(each logics))
}

// The inputs are read from opaque globals so both inlined instances (and hence
// both 'self' descriptions) survive optimization instead of being folded away.
public var seed = 21
public var text = "debug"

// CHECK: define {{.*}} @"$s1a3runSi_SityF"
public func run() -> (Int, Int) {
  let input = (Box(value: seed), Box(value: text))
  return packEngine(input, { $0 &* 2 }, { $0.count })
}

// The two inlined instances of the 'transform' specialization each get their
// own 'self' variable, in their own DISubprogram, describing the concrete type
// they were inlined with -- one 'Box<Int>' and one 'Box<String>' -- rather than
// collapsing onto a single shared 'self'.

// CHECK-DAG: ![[TRANSFORM_INT:[0-9]+]] = distinct !DISubprogram(name: "transform", {{.*}}DISPFlagDefinition
// CHECK-DAG: !DILocalVariable(name: "self", arg: 2, scope: ![[TRANSFORM_INT]], {{.*}}type: ![[SELF_INT:[0-9]+]]
// CHECK-DAG: ![[SELF_INT]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[LAYOUT_INT:[0-9]+]])
// CHECK-DAG: ![[LAYOUT_INT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}elements: ![[MEMBERS_INT:[0-9]+]]
// CHECK-DAG: ![[MEMBERS_INT]] = !{![[MEMBER_INT:[0-9]+]]}
// CHECK-DAG: ![[MEMBER_INT]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[BOX_INT:[0-9]+]]
// CHECK-DAG: ![[BOX_INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s1a3BoxVySiGD"

// CHECK-DAG: ![[TRANSFORM_STR:[0-9]+]] = distinct !DISubprogram(name: "transform", {{.*}}DISPFlagDefinition
// CHECK-DAG: !DILocalVariable(name: "self", arg: 2, scope: ![[TRANSFORM_STR]], {{.*}}type: ![[SELF_STR:[0-9]+]]
// CHECK-DAG: ![[SELF_STR]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[LAYOUT_STR:[0-9]+]])
// CHECK-DAG: ![[LAYOUT_STR]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}elements: ![[MEMBERS_STR:[0-9]+]]
// CHECK-DAG: ![[MEMBERS_STR]] = !{![[MEMBER_STR:[0-9]+]]}
// CHECK-DAG: ![[MEMBER_STR]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[BOX_STR:[0-9]+]]
// CHECK-DAG: ![[BOX_STR]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s1a3BoxVySSGD"

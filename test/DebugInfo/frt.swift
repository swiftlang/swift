// RUN: %target-swift-frontend -emit-ir -g %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple -o - | %FileCheck %s

// A variable of foreign reference type is described like one of class type:
// its location holds the reference, not the referenced object.

import ReferenceCounted

func use<T>(_ t: T) {}

public func variables(_ param: GlobalCount) {
  let local = GlobalCount.create()
  let maybe: GlobalCount? = local
  // GlobalCountNullableInit only appears inside an Optional.
  let nullable = GlobalCountNullableInit.create(false)
  use((param, local, maybe, nullable))
}

// CHECK-DAG: #dbg_declare(ptr %param.debug, ![[PARAM:[0-9]+]], !DIExpression(),
// CHECK-DAG: #dbg_declare(ptr %local.debug, ![[LOCAL:[0-9]+]], !DIExpression(),
// CHECK-DAG: #dbg_declare(ptr %maybe.debug, ![[MAYBE:[0-9]+]], !DIExpression(),
// CHECK-DAG: #dbg_declare(ptr %nullable.debug, ![[NULLABLE:[0-9]+]], !DIExpression(),

// CHECK-DAG: ![[PARAM]] = !DILocalVariable(name: "param", arg: 1,{{.*}} type: ![[CONST_FRT:[0-9]+]])
// CHECK-DAG: ![[LOCAL]] = !DILocalVariable(name: "local",{{.*}} type: ![[CONST_FRT]])
// CHECK-DAG: ![[CONST_FRT]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[FRT:[0-9]+]])

// CHECK-DAG: ![[MAYBE]] = !DILocalVariable(name: "maybe",{{.*}} type: ![[CONST_OPT:[0-9]+]])
// CHECK-DAG: ![[CONST_OPT]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPT_WRAPPER:[0-9]+]])
// CHECK-DAG: ![[OPT_WRAPPER]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[OPT_ELTS:[0-9]+]]
// CHECK-DAG: ![[OPT_ELTS]] = !{![[OPT_MEMBER:[0-9]+]]}
// CHECK-DAG: ![[OPT_MEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[OPT:[0-9]+]]
// CHECK-DAG: ![[OPT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSo11GlobalCountVSgD",{{.*}} templateParams: ![[OPT_PARAMS:[0-9]+]])
// CHECK-DAG: ![[OPT_PARAMS]] = !{![[OPT_PARAM:[0-9]+]]}
// CHECK-DAG: ![[OPT_PARAM]] = !DITemplateTypeParameter(type: ![[FRT]])

// Foreign reference types are mangled like C structs, so the type carries a
// flag that tells the debugger its values are references.
// CHECK-DAG: ![[FRT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GlobalCount",{{.*}} identifier: "$sSo11GlobalCountVD", annotations: ![[ANNOTS:[0-9]+]])
// CHECK-DAG: ![[ANNOTS]] = !{![[FRT_FLAG:[0-9]+]]}
// CHECK-DAG: ![[FRT_FLAG]] = !{!"swift.ForeignReferenceType", i1 true}

// The complete type, not a forward declaration, describes a foreign reference
// type that is only used as a generic argument.
// CHECK-DAG: ![[NULLABLE]] = !DILocalVariable(name: "nullable",{{.*}} type: ![[CONST_NULLABLE_OPT:[0-9]+]])
// CHECK-DAG: ![[CONST_NULLABLE_OPT]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[NULLABLE_OPT_WRAPPER:[0-9]+]])
// CHECK-DAG: ![[NULLABLE_OPT_WRAPPER]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[NULLABLE_OPT_ELTS:[0-9]+]]
// CHECK-DAG: ![[NULLABLE_OPT_ELTS]] = !{![[NULLABLE_OPT_MEMBER:[0-9]+]]}
// CHECK-DAG: ![[NULLABLE_OPT_MEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[NULLABLE_OPT:[0-9]+]]
// CHECK-DAG: ![[NULLABLE_OPT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSo23GlobalCountNullableInitVSgD",{{.*}} templateParams: ![[NULLABLE_OPT_PARAMS:[0-9]+]])
// CHECK-DAG: ![[NULLABLE_OPT_PARAMS]] = !{![[NULLABLE_OPT_PARAM:[0-9]+]]}
// CHECK-DAG: ![[NULLABLE_OPT_PARAM]] = !DITemplateTypeParameter(type: ![[NULLABLE_FRT:[0-9]+]])
// CHECK-DAG: ![[NULLABLE_FRT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GlobalCountNullableInit",{{.*}} identifier: "$sSo23GlobalCountNullableInitVD", annotations: ![[ANNOTS]])

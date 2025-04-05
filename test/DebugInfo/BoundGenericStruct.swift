// RUN: %target-swift-frontend %s -O -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s --check-prefix=DWARF
public struct S<T> {
  let t : T
}

public let s = S<Int>(t: 0)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct1SVySiGD",
// CHECK-SAME:             templateParams: ![[PARAMS:[0-9]+]]
// CHECK: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// CHECK: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}"$sSiD"

// DWARF-DAG: !DICompositeType(tag: DW_TAG_structure_type, {{.*}}name: "$s18BoundGenericStruct1SVySiGD"{{.*}}templateParams: ![[PARAMS:[0-9]+]]

// DWARF-DAG: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// DWARF-DAG: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// DWARF-DAG: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", {{.*}}"$sSiD"

// DWARF-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "S", {{.*}}identifier: "$s18BoundGenericStruct1SVyxGD")
// DWARF-DAG: !DIDerivedType(tag: DW_TAG_member, name: "t"


public struct S2<T> {
  public struct Inner {
    let t: T
  }
}
public let inner = S2<Double>.Inner(t:4.2)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "Inner", 
// CHECK-SAME: size: 64, {{.*}}identifier: "$s18BoundGenericStruct2S2V5InnerVySd_GD")
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct2S2VyxGD", 
// CHECK-SAME: flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)

// DWARF-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct2S2V5InnerVySd_GD", scope: ![[SCOPE1:[0-9]+]], {{.*}}flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: ![[PARAMS2:[0-9]+]])

// DWARF-DAG: ![[SCOPE1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct2S2VyxGD", 

// DWARF-DAG: ![[PARAMS2]] = !{![[PARAMS3:[0-9]+]]}
// DWARF-DAG: ![[PARAMS3]] = !DITemplateTypeParameter(type: ![[PARAMS4:[0-9]+]])
// DWARF-DAG: ![[PARAMS4]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Double"{{.*}} size: 64, {{.*}}runtimeLang: DW_LANG_Swift,{{.*}} identifier: "$sSdD")

// DWARF-DAG: ![[SPECIFICATION:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Inner", {{.*}}elements: ![[ELEMENTS1:[0-9]+]], runtimeLang: DW_LANG_Swift, identifier: "$s18BoundGenericStruct2S2V5InnerVyx_GD")
// DWARF-DAG: !DICompositeType(tag: DW_TAG_structure_type, {{.*}}line: 26, size: 64, {{.*}}specification: ![[SPECIFICATION]]

// DWARF-DAG: ![[ELEMENTS1]] = !{![[ELEMENTS2:[0-9]+]]}

// DWARF-DAG: ![[ELEMENTS2]] = !DIDerivedType(tag: DW_TAG_member, name: "t"

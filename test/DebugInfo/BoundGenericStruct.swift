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
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSiD",


// DWARF: !DICompositeType(tag: DW_TAG_structure_type,
// DWARF-SAME:             templateParams: ![[PARAMS:[0-9]+]]
// DWARF-SAME:             identifier: "$s18BoundGenericStruct1SVySiGD"
// DWARF-SAME:             specification_of:

// DWARF: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// DWARF: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// DWARF: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", {{.*}}identifier: "$sSiD"

// DWARF: !DICompositeType(tag: DW_TAG_structure_type, name: "S", 
// DWARF-SAME:             identifier: "$s18BoundGenericStruct1SVyxGD")
// DWARF: !DIDerivedType(tag: DW_TAG_member, name: "t"
// DWARF: ![[GENERIC_PARAM_TYPE:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxD"


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

// DWARF: !DICompositeType(tag: DW_TAG_structure_type, scope: ![[SCOPE1:[0-9]+]],
// DWARF-SAME: size: 64, {{.*}}, templateParams: ![[PARAMS2:[0-9]+]], identifier: "$s18BoundGenericStruct2S2V5InnerVySd_GD"
// DWARF-SAME: specification_of: ![[SPECIFICATION:[0-9]+]]

// DWARF: ![[SCOPE1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct2S2VyxGD", 

// DWARF: ![[PARAMS2]] = !{![[PARAMS3:[0-9]+]]}
// DWARF: ![[PARAMS3]] = !DITemplateTypeParameter(type: ![[PARAMS4:[0-9]+]])
// DWARF: ![[PARAMS4]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Double",
// DWARF-SAME: size: 64, {{.*}}runtimeLang: DW_LANG_Swift, identifier: "$sSdD")

// DWARF: [[SPECIFICATION]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Inner", 
// DWARF-SAME: elements: ![[ELEMENTS1:[0-9]+]], runtimeLang: DW_LANG_Swift, identifier: "$s18BoundGenericStruct2S2V5InnerVyx_GD")

// DWARF: ![[ELEMENTS1]] = !{![[ELEMENTS2:[0-9]+]]}

// DWARF: ![[ELEMENTS2]] = !DIDerivedType(tag: DW_TAG_member, name: "t", scope: !27, file: !3, baseType: ![[GENERIC_PARAM_TYPE]])

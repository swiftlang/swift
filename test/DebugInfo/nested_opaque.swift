// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

protocol TheProtocol {
}

// CHECK: ![[FUNC_ID:[0-9]+]] = distinct !DISubprogram(name: "theFunction", 
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "TheType", scope: ![[FUNC_ID]]
func theFunction() -> some TheProtocol {
    struct TheType: TheProtocol {
    }
    return TheType()
}

theFunction()

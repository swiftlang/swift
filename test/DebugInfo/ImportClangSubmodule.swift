// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - | %FileCheck %s

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "Bar",
// CHECK-SAME:             scope: ![[SUBMODULE:[0-9]+]]

// CHECK: ![[SUBMODULE]] = !DIModule(scope: ![[CLANGMODULE:[0-9]+]],
// CHECK-SAME:                       name: "SubModule",
// CHECK: ![[CLANGMODULE]] = !DIModule(scope: null, name: "ClangModule",
// CHECK-SAME:                         configMacros:
// CHECK-SAME:                         {{..}}-DFOO=foo{{..}}
// CHECK-SAME:                         {{..}}-UBAR{{..}}
// CHECK: !DIImportedEntity({{.*}}, entity: ![[SUBMODULE]], line: [[@LINE+1]])
import ClangModule.SubModule

let bar = Bar()

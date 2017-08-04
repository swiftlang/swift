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

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "OtherBar",
// CHECK-SAME:             scope: ![[OTHERSUBMODULE:[0-9]+]]
// CHECK: ![[OTHERSUBMODULE]] = !DIModule(scope: ![[OTHERCLANGMODULE:[0-9]+]],

// CHECK-SAME:                       name: "SubModule",
// CHECK: ![[OTHERCLANGMODULE]] = !DIModule(scope: null,
// CHECK-SAME:                              name: "OtherClangModule",
// CHECK-SAME:                              configMacros:
// CHECK-SAME:                              {{..}}-DFOO=foo{{..}}
// CHECK-SAME:                              {{..}}-UBAR{{..}}

// CHECK: !DIImportedEntity({{.*}}, entity: ![[SUBMODULE]], file:
// CHECK-SAME:              line: [[@LINE+1]])
import ClangModule.SubModule
// CHECK: !DIImportedEntity({{.*}}, entity: ![[OTHERSUBMODULE]],
// CHECK-SAME:              line: [[@LINE+1]])
import OtherClangModule.SubModule

// The Swift compiler uses an ugly hack that auto-imports a
// submodule's top-level-module, even if we didn't ask for it.
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[SUBMODULE]]
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[OTHERSUBMODULE]]
// CHECK: !DIImportedEntity({{.*}}, entity: ![[CLANGMODULE]]
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[SUBMODULE]]
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[OTHERSUBMODULE]]
// CHECK: !DIImportedEntity({{.*}}, entity: ![[OTHERCLANGMODULE]]
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[SUBMODULE]]
// CHECK-NOT: !DIImportedEntity({{.*}}, entity: ![[OTHERSUBMODULE]]

let bar = Bar()
let baz = OtherBar()

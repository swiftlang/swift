// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - | %FileCheck %s
//
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - -no-clang-module-breadcrumbs \
// RUN:   | %FileCheck %s --check-prefix=NONE
//
// RUN: %target-swift-frontend -module-cache-path %t.mcp -emit-ir %s \
// RUN:   -g -I %S/Inputs -Xcc -DFOO="foo" -Xcc -UBAR \
// RUN:   -debug-prefix-map %t.mcp=PREFIX \
// RUN:   -o - | %FileCheck %s --check-prefix=REMAP

import ClangModule.SubModule
import OtherClangModule.SubModule

let _ = someFunc(0)

// Check for Clang module breadcrumbs.
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}},{{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           ClangModule
// CHECK-SAME:           dwoId:

// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}}, {{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           OtherClangModule
// CHECK-SAME:           dwoId:

// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}},{{.*}} producer: "{{.*}}clang
// CHECK-SAME:           ClangModule
// CHECK-SAME:           dwoId:

// NONE: DICompileUnit({{.*}}
// NONE-NOT: DICompileUnit({{.*}}ClangModule

// REMAP: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}},{{.*}} producer: "{{.*}}Swift
// REMAP-SAME:           PREFIX{{/|\\\\}}{{.*}}{{/|\\\\}}ClangModule
// REMAP-SAME:           dwoId:

// REMAP: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}},{{.*}} producer: "{{.*}}Swift
// REMAP-SAME:           PREFIX{{/|\\\\}}{{.*}}{{/|\\\\}}OtherClangModule
// REMAP-SAME:           dwoId:

// REMAP: !DICompileUnit(language: DW_LANG_{{ObjC|C99|C11}},{{.*}} producer: "{{.*}}clang
// REMAP-SAME:           PREFIX{{/|\\\\}}{{.*}}{{/|\\\\}}ClangModule
// REMAP-SAME:           dwoId:

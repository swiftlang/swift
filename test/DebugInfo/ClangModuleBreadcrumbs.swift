// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - | %FileCheck %s
//
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - -no-clang-module-breadcrumbs \
// RUN:   | %FileCheck %s --check-prefix=NONE
import ClangModule.SubModule
import OtherClangModule.SubModule

// Check for Clang module breadcrumbs.
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}},{{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           ClangModule
// CHECK-SAME:           dwoId:

// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}}, {{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           OtherClangModule
// CHECK-SAME:           dwoId:

// NONE: DICompileUnit({{.*}}
// NONE-NOT: DICompileUnit({{.*}}ClangModule

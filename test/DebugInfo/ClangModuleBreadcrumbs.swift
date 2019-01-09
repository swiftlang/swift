// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs \
// RUN:   -Xcc -DFOO="foo" -Xcc -UBAR -o - | %FileCheck %s
import ClangModule.SubModule
import OtherClangModule.SubModule

// Check for Clang module breadcrumbs.
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}},{{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           ClangModule
// CHECK-SAME:           dwoId:

// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}}, {{.*}} producer: "{{.*}}Swift
// CHECK-SAME:           OtherClangModule
// CHECK-SAME:           dwoId:

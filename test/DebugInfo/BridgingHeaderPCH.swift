// RUN: %target-swift-frontend \
// RUN:   -emit-pch %S/Inputs/InlineBridgingHeader.h -o %t.pch 
// RUN: %target-swift-frontend \
// RUN:   -import-objc-header %t.pch -emit-ir -g %s -o - | %FileCheck %s

// CHECK: !DICompileUnit(language: DW_LANG_Swift
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}},
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}},
// CHECK-SAME:           splitDebugFilename: "{{.*}}.pch"
// CHECK-SAME:           dwoId:

Foo()

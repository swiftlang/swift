// RUN: %target-swift-frontend \
// RUN:   -emit-pch %S/Inputs/BridgingHeader.h -o %t.pch
// RUN: %target-swift-frontend \
// RUN:   -import-objc-header %t.pch -emit-ir -g %s -o - | %FileCheck %s

// CHECK: !DIModule(scope: null, name: "BridgingHeader.h",
// CHECK: !DICompileUnit(language: DW_LANG_{{ObjC|C99}},{{.*}}splitDebugFilename:
// CHECK-SAME:           dwoId:

public let p = Point(x: 1, y: 2)

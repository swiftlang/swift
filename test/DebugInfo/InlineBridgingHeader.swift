// RUN: %target-swift-frontend \
// RUN:   -import-objc-header %S/Inputs/InlineBridgingHeader.h \
// RUN:   -emit-ir -g %s -o - | %FileCheck %s

// The Swift CU must come first.
// CHECK: !llvm.dbg.cu = !{![[SWIFT_CU:[0-9]+]], ![[CLANG_CU:[0-9]+]], ![[CONCURRENCY_SHIMS_CU:[0-9]+]]}
// CHECK: ![[SWIFT_CU]] = distinct !DICompileUnit(language: DW_LANG_Swift
// CHECK: ![[CLANG_CU]] = distinct !DICompileUnit(
// CHECK-SAME:                          language: {{DW_LANG_ObjC|DW_LANG_C}}
// CHECK: ![[CONCURRENCY_SHIMS_CU]] = distinct !DICompileUnit(
// CHECK-SAME:                          language: {{DW_LANG_ObjC|DW_LANG_C}}
// CHECK: DISubprogram(name: "Foo"{{.*}} unit: ![[CLANG_CU]]

Foo()

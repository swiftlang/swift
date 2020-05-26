// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs | %FileCheck %s
import ClangModule

func use<T>(_ t: T) {}

let foo = s_Foo()
// CHECK: !DIDerivedType(tag: DW_TAG_typedef,
// CHECK-SAME:           scope: ![[CLANG_MODULE:[0-9]+]],
// CHECK: ![[CLANG_MODULE]] = !DIModule(scope: null, name: "ClangModule"
use(foo)

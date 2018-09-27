// RUN: %target-swift-ide-test -print-indexed-symbols -enable-source-import -source-filename %s -I %S/Store/Inputs | %FileCheck %s

import ClangModuleB
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleB | c:@M@ClangModuleB | Ref | rel: 0
import ClangModuleC.Sub1
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | c:@M@ClangModuleC | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | ClangModuleC.Sub1 | c:@M@ClangModuleC@M@Sub1 | Ref | rel: 0
import ClangModuleC.Sub2
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | c:@M@ClangModuleC | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | ClangModuleC.Sub2 | c:@M@ClangModuleC@M@Sub2 | Ref | rel: 0
import func ClangModuleA.funcA
// CHECK: [[@LINE-1]]:13 | module/C | ClangModuleA | c:@M@ClangModuleA | Ref | rel: 0
// CHECK: [[@LINE-2]]:26 | function/Swift | funcA() | c:@F@funcA | Ref | rel: 0
import SwiftModuleC
// CHECK: [[@LINE-1]]:8 | module/Swift | SwiftModuleC | c:@M@SwiftModuleC | Ref | rel: 0

struct MyType {}

extension SwiftModuleC.MyType {
  // CHECK: [[@LINE-1]]:11 | module/Swift | SwiftModuleC | c:@M@SwiftModuleC | Ref | rel: 0
  func myMethod() {
    _ = SwiftModuleC.MyGeneric<SwiftModuleC.MyType, MyType>()
    // CHECK: [[@LINE-1]]:9 | module/Swift | SwiftModuleC | c:@M@SwiftModuleC | Ref | rel: 0
    // CHECK: [[@LINE-2]]:22 | struct/Swift | MyGeneric | s:12SwiftModuleC9MyGenericV | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-3]]:32 | module/Swift | SwiftModuleC | c:@M@SwiftModuleC | Ref | rel: 0
    // CHECK: [[@LINE-4]]:45 | struct/Swift | MyType | s:12SwiftModuleC6MyTypeV | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-5]]:53 | struct/Swift | MyType | s:14swift_ide_test6MyTypeV | Ref,RelCont | rel: 1
  }
}


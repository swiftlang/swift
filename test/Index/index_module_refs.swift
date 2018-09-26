// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -I %S/Store/Inputs | %FileCheck %s

import Foundation
// CHECK: [[@LINE-1]]:8 | module/Swift | Foundation | c:@M@Foundation | Ref | rel: 0
import ClangModuleA
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleA | c:@M@ClangModuleA | Ref | rel: 0
import ClangModuleC.Sub1
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | c:@M@ClangModuleC | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | Sub1 | c:@M@ClangModuleC@M@Sub1 | Ref | rel: 0
import ClangModuleC.Sub2
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | c:@M@ClangModuleC | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | Sub2 | c:@M@ClangModuleC@M@Sub2 | Ref | rel: 0
import func Darwin.glob
// CHECK: [[@LINE-1]]:13 | module/Swift | Darwin | c:@M@Darwin | Ref | rel: 0

struct Array {}

extension Swift.Array {
// CHECK: [[@LINE-1]]:11 | module/Swift | Swift | c:@M@Swift | Ref | rel: 0
  func myMethod() {
    let dict = Swift.Dictionary<Swift.String, Array>()
    // CHECK: [[@LINE-1]]:16 | module/Swift | Swift | c:@M@Swift | Ref | rel: 0
    // CHECK: [[@LINE-2]]:22 | struct/Swift | Dictionary | s:SD | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-3]]:33 | module/Swift | Swift | c:@M@Swift | Ref | rel: 0
    // CHECK: [[@LINE-4]]:39 | struct/Swift | String | s:SS | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-5]]:47 | struct/Swift | Array | s:14swift_ide_test5ArrayV | Ref,RelCont | rel: 1
  }
}

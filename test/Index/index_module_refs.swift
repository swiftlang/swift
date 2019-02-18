// RUN: %target-swift-ide-test -print-indexed-symbols -enable-source-import -source-filename %s -I %S/Store/Inputs | %FileCheck %s

import NonExistingModuleName // Make sure invalid imports aren't affecting results
// CHECK-NOT: {{.*}} | NonExistingModuleName

import ClangModuleB
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleB | c:@M@ClangModuleB | Ref | rel: 0
import ClangModuleC.Sub1
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | [[ClangModuleC_USR:c:@M@ClangModuleC]] | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | ClangModuleC.Sub1 | [[ClangModuleC_USR]]@M@Sub1 | Ref | rel: 0
import ClangModuleC.Sub2
// CHECK: [[@LINE-1]]:8 | module/C | ClangModuleC | [[ClangModuleC_USR]] | Ref | rel: 0
// CHECK: [[@LINE-2]]:21 | module/C | ClangModuleC.Sub2 | [[ClangModuleC_USR]]@M@Sub2 | Ref | rel: 0
import func ClangModuleA.funcA
// CHECK: [[@LINE-1]]:13 | module/C | ClangModuleA | c:@M@ClangModuleA | Ref | rel: 0
import SwiftModuleC
// CHECK: [[@LINE-1]]:8 | module/Swift | SwiftModuleC | [[SwiftModuleC_USR:c:@M@SwiftModuleC]] | Ref | rel: 0

struct MyType {}

extension SwiftModuleC.MyType {
  // CHECK: [[@LINE-1]]:11 | module/Swift | SwiftModuleC | [[SwiftModuleC_USR]] | Ref | rel: 0
  func myMethod() {
    _ = SwiftModuleC.MyGeneric<SwiftModuleC.MyType, MyType>()
    // CHECK: [[@LINE-1]]:9 | module/Swift | SwiftModuleC | [[SwiftModuleC_USR]] | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-method/Swift | myMethod() | [[myMethod_USR:.*]]
    // CHECK: [[@LINE-3]]:22 | struct/Swift | MyGeneric | {{.*}} | Ref
    // CHECK: [[@LINE-4]]:32 | module/Swift | SwiftModuleC | [[SwiftModuleC_USR]] | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-method/Swift | myMethod() | [[myMethod_USR]]
    // CHECK: [[@LINE-6]]:45 | struct/Swift | MyType | {{.*}} | Ref
  }
}

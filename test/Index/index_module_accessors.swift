// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Mod.swiftmodule -module-name Mod %s
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print Mod -source-filename %s -I %t | %FileCheck %s

// rdar://130775560 - Make sure the accessors are marked implicit in the index
// data for the module.
public struct S {
  // CHECK-DAG: instance-property/Swift | x | s:3Mod1SV1xSivp | Def,RelChild
  // CHECK-DAG: instance-method/acc-get/Swift | getter:x | s:3Mod1SV1xSivg | Def,Impl,RelChild,RelAcc
  public let x = 0

  // CHECK-DAG: instance-property/Swift | y | s:3Mod1SV1ySivp | Def,RelChild
  // CHECK-DAG: instance-method/acc-get/Swift | getter:y | s:3Mod1SV1ySivg | Def,Impl,RelChild,RelAcc
  public var y: Int {
    0
  }

  // CHECK-DAG: instance-property/Swift | z | s:3Mod1SV1zSivp | Def,RelChild
  // CHECK-DAG: instance-method/acc-get/Swift | getter:z | s:3Mod1SV1zSivg | Def,Impl,RelChild,RelAcc
  // CHECK-DAG: instance-method/acc-set/Swift | setter:z | s:3Mod1SV1zSivs | Def,Impl,RelChild,RelAcc
  public var z: Int {
    get { 0 }
    set {}
  }

  // CHECK-DAG: instance-property/Swift | a | s:3Mod1SV1aSivp | Def,RelChild
  // CHECK-DAG: instance-method/acc-get/Swift | getter:a | s:3Mod1SV1aSivg | Def,Impl,RelChild,RelAcc
  public var a: Int {
    @inlinable
    get { 0 }
  }
}

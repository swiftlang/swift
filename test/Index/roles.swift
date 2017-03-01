// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/imported_swift_module.swift
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -I %t | %FileCheck %s

import func imported_swift_module.importedFunc
// CHECK: [[@LINE-1]]:35 | function/Swift | importedFunc() | s:21imported_swift_module0A4FuncyyF | Ref | rel: 0
import var imported_swift_module.importedGlobal
// CHECK: [[@LINE-1]]:34 | variable/Swift | importedGlobal | s:21imported_swift_module0A6GlobalSiv | Ref | rel: 0

// Definition
let x = 2
// CHECK: [[@LINE-1]]:5 | variable/Swift | x | s:14swift_ide_test1xSiv | Def | rel: 0

// Definition + Read of x
var y = x + 1
// CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:14swift_ide_test1ySiv | Def | rel: 0
// CHECK: [[@LINE-2]]:9 | variable/Swift | x | s:14swift_ide_test1xSiv | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:11 | function/infix-operator/Swift | +(_:_:) | s:s1poiSiSi_SitF | Ref,Call | rel: 0

// Read of x + Write of y
y = x + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:14swift_ide_test1ySiv | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:5 | variable/Swift | x | s:14swift_ide_test1xSiv | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:7 | function/infix-operator/Swift | +(_:_:) | s:s1poiSiSi_SitF | Ref,Call | rel: 0

// Read of y + Write of y
y += x
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:14swift_ide_test1ySiv | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-2]]:3 | function/infix-operator/Swift | +=(_:_:) | s:s2peoiySiz_SitF | Ref,Call | rel: 0
// CHECK: [[@LINE-3]]:6 | variable/Swift | x | s:14swift_ide_test1xSiv | Ref,Read | rel: 0

var z: Int {
// CHECK: [[@LINE-1]]:5 | variable/Swift | z | s:14swift_ide_test1zSiv | Def | rel: 0
  get {
    // CHECK: [[@LINE-1]]:3 | function/acc-get/Swift | getter:z | s:14swift_ide_test1zSifg | Def,RelChild,RelAcc | rel: 1

    return y
    // CHECK: [[@LINE-1]]:12 | variable/Swift | y | s:14swift_ide_test1ySiv | Ref,Read | rel: 0
  }
  set {
    // CHECK: [[@LINE-1]]:3 | function/acc-set/Swift | setter:z | s:14swift_ide_test1zSifs | Def,RelChild,RelAcc | rel: 1

    y = newValue
    // CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:14swift_ide_test1ySiv | Ref,Writ | rel: 0
  }
}
// Write + Read of z
z = z + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | z | s:14swift_ide_test1zSiv | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:1 | function/acc-set/Swift | setter:z | s:14swift_ide_test1zSifs | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE-3]]:5 | variable/Swift | z | s:14swift_ide_test1zSiv | Ref,Read | rel: 0
// CHECK: [[@LINE-4]]:5 | function/acc-get/Swift | getter:z | s:14swift_ide_test1zSifg | Ref,Call,Impl | rel: 0

// Call
func aCalledFunction(a: Int, b: inout Int) {
// CHECK: [[@LINE-1]]:6 | function/Swift | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF | Def | rel: 0
// CHECK: [[@LINE-2]]:22 | param/Swift | a | s:{{.*}} | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF
// CHECK: [[@LINE-4]]:30 | param/Swift | b | s:{{.*}} | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF

  var _ = a + b
  // CHECK: [[@LINE-1]]:11 | param/Swift | a | s:{{.*}} | Ref,Read | rel: 0
  // CHECK: [[@LINE-2]]:15 | param/Swift | b | s:{{.*}} | Ref,Read | rel: 0

  b = a + 1
  // CHECK: [[@LINE-1]]:3 | param/Swift | b | s:{{.*}} | Ref,Writ | rel: 0
  // CHECK: [[@LINE-2]]:7 | param/Swift | a | s:{{.*}} | Ref,Read | rel: 0
}

aCalledFunction(a: 1, b: &z)
// CHECK: [[@LINE-1]]:1 | function/Swift | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF | Ref,Call | rel: 0
// CHECK: [[@LINE-2]]:27 | variable/Swift | z | s:14swift_ide_test1zSiv | Ref,Read,Writ | rel: 0

func aCaller() {
  // CHECK: [[@LINE-1]]:6 | function/Swift | aCaller() | s:14swift_ide_test7aCalleryyF | Def | rel: 0

  aCalledFunction(a: 1, b: &z)
  // CHECK: [[@LINE-1]]:3 | function/Swift | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF | Ref,Call,RelCall | rel: 1
  // CHECK-NEXT: RelCall | aCaller() | s:14swift_ide_test7aCalleryyF
}

let _ = aCalledFunction
// CHECK: [[@LINE-1]]:9 | function/Swift | aCalledFunction(a:b:) | s:14swift_ide_test15aCalledFunctionySi1a_Siz1btF | Ref | rel: 0

// RelationChildOf, Implicit
struct AStruct {
  var x: Int
  // CHECK: [[@LINE-1]]:7 | instance-property/Swift | x | s:14swift_ide_test7AStructV1xSiv | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AStruct | s:14swift_ide_test7AStructV

  mutating func aMethod() {
    // CHECK: [[@LINE-1]]:17 | instance-method/Swift | aMethod() | s:14swift_ide_test7AStructV7aMethodyyF | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:14swift_ide_test7AStructV

    x += 1
    // CHECK: [[@LINE-1]]:5 | instance-property/Swift | x | s:14swift_ide_test7AStructV1xSiv | Ref,Read,Writ | rel: 0
    // CHECK: [[@LINE-2]]:5 | function/acc-get/Swift | getter:x | s:14swift_ide_test7AStructV1xSifg | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | aMethod() | s:14swift_ide_test7AStructV7aMethodyyF
    // CHECK-NEXT: RelRec | AStruct | s:14swift_ide_test7AStructV
    // CHECK: [[@LINE-5]]:5 | function/acc-set/Swift | setter:x | s:14swift_ide_test7AStructV1xSifs | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | aMethod() | s:14swift_ide_test7AStructV7aMethodyyF
    // CHECK-NEXT: RelRec | AStruct | s:14swift_ide_test7AStructV
    // CHECK: [[@LINE-8]]:7 | function/infix-operator/Swift | +=(_:_:) | s:s2peoiySiz_SitF | Ref,Call,RelCall | rel: 1
    // CHECK-NEXT: RelCall | aMethod() | s:14swift_ide_test7AStructV7aMethodyyF
  }

  // RelationChildOf, RelationAccessorOf
  subscript(index: Int) -> Int {
    // CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSici | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:14swift_ide_test7AStructV

    get {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-get/Swift | getter:subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSicfg | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSici

      return x
    }
    set {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-set/Swift | setter:subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSicfs | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSici

      x = newValue
    }
  }
}

class AClass { // used for references only
  var y: AStruct;
  var z: [Int]
  init(x: Int) {
    y = AStruct(x: x)
    self.z = [1, 2, 3]
  }
  subscript(index: Int) -> Int {
    get { return z[0] }
    set { z[0] = newValue }
  }
  func foo() -> Int { return z[0] }
}

let _ = AClass.foo
// CHECK: [[@LINE-1]]:16 | instance-method/Swift | foo() | s:14swift_ide_test6AClassC3fooSiyF | Ref | rel: 0
let _ = AClass(x: 1).foo
// CHECK: [[@LINE-1]]:22 | instance-method/Swift | foo() | s:14swift_ide_test6AClassC3fooSiyF | Ref | rel: 0
let _ = AClass(x: 1)[1]
// CHECK: [[@LINE-1]]:21 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test6AClassC9subscriptSiSici | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:21 | function/acc-get/Swift | getter:subscript(_:) | s:14swift_ide_test6AClassC9subscriptSiSicfg | Ref,Call,Dyn,Impl,RelRec | rel: 1
let _ = AClass(x: 1)[1] = 2
// CHECK: [[@LINE-1]]:21 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test6AClassC9subscriptSiSici | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:21 | function/acc-set/Swift | setter:subscript(_:) | s:14swift_ide_test6AClassC9subscriptSiSicfs | Ref,Call,Dyn,Impl,RelRec | rel: 1

// RelationBaseOf, RelationOverrideOf

protocol X {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | X | [[X_USR:.*]] | Def | rel: 0

class ImplementsX : X {}
// CHECK: [[@LINE-1]]:7 | class/Swift | ImplementsX | [[ImplementsX_USR:.*]] | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | X | [[X_USR]] | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | ImplementsX | [[ImplementsX_USR]]

protocol AProtocol {
  // CHECK: [[@LINE-1]]:10 | protocol/Swift | AProtocol | [[AProtocol_USR:.*]] | Def | rel: 0

  associatedtype T : X
  // CHECK: [[@LINE-1]]:18 | type-alias/associated-type/Swift | T | s:14swift_ide_test9AProtocolP1T | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AProtocol | [[AProtocol_USR]]
  // CHECK: [[@LINE-3]]:22 | protocol/Swift | X | [[X_USR]] | Ref | rel: 0

  func foo() -> Int
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | foo() | s:14swift_ide_test9AProtocolP3fooSiyF | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AProtocol | s:14swift_ide_test9AProtocolP
}

class ASubClass : AClass, AProtocol {
// CHECK: [[@LINE-1]]:7 | class/Swift | ASubClass | s:14swift_ide_test9ASubClassC | Def | rel: 0
// CHECK: [[@LINE-2]]:19 | class/Swift | AClass | s:14swift_ide_test6AClassC | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | ASubClass | s:14swift_ide_test9ASubClassC
// CHECK: [[@LINE-4]]:27 | protocol/Swift | AProtocol | s:14swift_ide_test9AProtocolP | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | ASubClass | s:14swift_ide_test9ASubClassC

  typealias T = ImplementsX

  override func foo() -> Int {
    // CHECK: [[@LINE-1]]:17 | instance-method/Swift | foo() | s:14swift_ide_test9ASubClassC3fooSiyF | Def,RelChild,RelOver | rel: 3
    // CHECK-NEXT: RelOver | foo() | s:14swift_ide_test6AClassC3fooSiyF
    // CHECK-NEXT: RelOver | foo() | s:14swift_ide_test9AProtocolP3fooSiyF
    // CHECK-NEXT: RelChild | ASubClass | s:14swift_ide_test9ASubClassC
    return 1
  }
}

// RelationExtendedBy
extension AClass {
  // CHECK: [[@LINE-1]]:11 | extension/ext-class/Swift | AClass | [[EXT_ACLASS_USR:.*]] | Def | rel: 0
  // CHECK: [[@LINE-2]]:11 | class/Swift | AClass | s:14swift_ide_test6AClassC | Ref,RelExt | rel: 1
  // CHECK-NEXT: RelExt | AClass | [[EXT_ACLASS_USR]]

  func bar() -> Int { return 2 }
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | bar() | s:14swift_ide_test6AClassC3barSiyF | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | [[EXT_ACLASS_USR]]
}

struct OuterS {
// CHECK: [[@LINE-1]]:8 | struct/Swift | OuterS | [[OUTERS_USR:.*]] | Def | rel: 0
  struct InnerS {}
  // CHECK: [[@LINE-1]]:10 | struct/Swift | InnerS | [[INNERS_USR:.*]] | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | OuterS | [[OUTERS_USR]]
}
extension OuterS.InnerS : AProtocol {
  // CHECK: [[@LINE-1]]:18 | extension/ext-struct/Swift | InnerS | [[EXT_INNERS_USR:.*]] | Def | rel: 0
  // CHECK: [[@LINE-2]]:18 | struct/Swift | InnerS | [[INNERS_USR]] | Ref,RelExt | rel: 1
  // CHECK-NEXT: RelExt | InnerS | [[EXT_INNERS_USR]]
  // CHECK: [[@LINE-4]]:27 | protocol/Swift | AProtocol | [[AProtocol_USR]] | Ref,RelBase | rel: 1
  // CHECK-NEXT: RelBase | InnerS | [[EXT_INNERS_USR]]
  // CHECK: [[@LINE-6]]:11 | struct/Swift | OuterS | [[OUTERS_USR]] | Ref | rel: 0

  typealias T = ImplementsX
  func foo() -> Int { return 1 }
}

var anInstance = AClass(x: 1)
// CHECK: [[@LINE-1]]:18 | class/Swift | AClass | s:14swift_ide_test6AClassC | Ref | rel: 0
// CHECK: [[@LINE-2]]:18 | constructor/Swift | init(x:) | s:14swift_ide_test6AClassCACSi1x_tcfc | Ref,Call | rel: 0

anInstance.y.x = anInstance.y.x
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | y | s:14swift_ide_test6AClassC1yAA7AStructVv | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:14 | instance-property/Swift | x | s:14swift_ide_test7AStructV1xSiv | Ref,Writ | rel: 0
// CHECK: [[@LINE-4]]:18 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-5]]:29 | instance-property/Swift | y | s:14swift_ide_test6AClassC1yAA7AStructVv | Ref,Read | rel: 0
// CHECK: [[@LINE-6]]:31 | instance-property/Swift | x | s:14swift_ide_test7AStructV1xSiv | Ref,Read | rel: 0

anInstance.y.aMethod()
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | y | s:14swift_ide_test6AClassC1yAA7AStructVv | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:14 | instance-method/Swift | aMethod() | s:14swift_ide_test7AStructV7aMethodyyF | Ref,Call | rel: 0

// FIXME Write role of z occurrence on the RHS?
anInstance.z[1] = anInstance.z[0]
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | z | s:14swift_ide_test6AClassC1zSaySiGv | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:19 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-4]]:30 | instance-property/Swift | z | s:14swift_ide_test6AClassC1zSaySiGv | Ref,Read,Writ | rel: 0

let otherInstance = AStruct(x: 1)
// CHECK: [[@LINE-1]]:21 | struct/Swift | AStruct | s:14swift_ide_test7AStructV | Ref | rel: 0

let _ = otherInstance[0]
// CHECK: [[@LINE-1]]:9 | variable/Swift | otherInstance | s:14swift_ide_test13otherInstanceAA7AStructVv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:22 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test7AStructV9subscriptSiSici | Ref,Read | rel: 0

let _ = anInstance[0]
// CHECK: [[@LINE-1]]:9 | variable/Swift | anInstance | s:14swift_ide_test10anInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:19 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test6AClassC9subscriptSiSici | Ref,Read | rel: 0

let aSubInstance: AClass = ASubClass(x: 1)
// CHECK: [[@LINE-1]]:5 | variable/Swift | aSubInstance | s:14swift_ide_test12aSubInstanceAA6AClassCv | Def | rel: 0
// CHECK: [[@LINE-2]]:28 | class/Swift | ASubClass | s:14swift_ide_test9ASubClassC | Ref | rel: 0

// Dynamic, RelationReceivedBy
let _ = aSubInstance.foo()
// CHECK: [[@LINE-1]]:9 | variable/Swift | aSubInstance | s:14swift_ide_test12aSubInstanceAA6AClassCv | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:22 | instance-method/Swift | foo() | s:14swift_ide_test6AClassC3fooSiyF | Ref,Call,Dyn,RelRec | rel: 1
// CHECK-NEXT: RelRec | AClass | s:14swift_ide_test6AClassC


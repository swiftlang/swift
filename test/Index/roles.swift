// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/imported_swift_module.swift
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -I %t | %FileCheck %s

import func imported_swift_module.importedFunc
// CHECK: [[@LINE-1]]:35 | function/Swift | importedFunc() | s:F21imported_swift_module12importedFuncFT_T_ | Ref | rel: 0
import var imported_swift_module.importedGlobal
// CHECK: [[@LINE-1]]:34 | variable/Swift | importedGlobal | s:v21imported_swift_module14importedGlobalSi | Ref | rel: 0

// Definition
let x = 2
// CHECK: [[@LINE-1]]:5 | variable/Swift | x | s:v14swift_ide_test1xSi | Def | rel: 0

// Definition + Read of x
var y = x + 1
// CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:v14swift_ide_test1ySi | Def | rel: 0
// CHECK: [[@LINE-2]]:9 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:11 | function/infix-operator/Swift | +(_:_:) | s:Fsoi1pFTSiSi_Si | Ref,Call | rel: 0

// Read of x + Write of y
y = x + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:5 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:7 | function/infix-operator/Swift | +(_:_:) | s:Fsoi1pFTSiSi_Si | Ref,Call | rel: 0

// Read of y + Write of y
y += x
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-2]]:3 | function/infix-operator/Swift | +=(_:_:) | s:Fsoi2peFTRSiSi_T_ | Ref,Call | rel: 0
// CHECK: [[@LINE-3]]:6 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0

var z: Int {
// CHECK: [[@LINE-1]]:5 | variable/Swift | z | s:v14swift_ide_test1zSi | Def | rel: 0
  get {
    // CHECK: [[@LINE-1]]:3 | function/acc-get/Swift | getter:z | s:F14swift_ide_testg1zSi | Def,RelChild,RelAcc | rel: 1

    return y
    // CHECK: [[@LINE-1]]:12 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Read | rel: 0
  }
  set {
    // CHECK: [[@LINE-1]]:3 | function/acc-set/Swift | setter:z | s:F14swift_ide_tests1zSi | Def,RelChild,RelAcc | rel: 1

    y = newValue
    // CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Writ | rel: 0
  }
}
// Write + Read of z
z = z + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | z | s:v14swift_ide_test1zSi | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:1 | function/acc-set/Swift | setter:z | s:F14swift_ide_tests1zSi | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE-3]]:5 | variable/Swift | z | s:v14swift_ide_test1zSi | Ref,Read | rel: 0
// CHECK: [[@LINE-4]]:5 | function/acc-get/Swift | getter:z | s:F14swift_ide_testg1zSi | Ref,Call,Impl | rel: 0

// Call
func aCalledFunction() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | aCalledFunction() | s:F14swift_ide_test15aCalledFunctionFT_T_ | Def | rel: 0

aCalledFunction()
// CHECK: [[@LINE-1]]:1 | function/Swift | aCalledFunction() | s:F14swift_ide_test15aCalledFunctionFT_T_ | Ref,Call | rel: 0

func aCaller() {
  // CHECK: [[@LINE-1]]:6 | function/Swift | aCaller() | s:F14swift_ide_test7aCallerFT_T_ | Def | rel: 0

  aCalledFunction()
  // CHECK: [[@LINE-1]]:3 | function/Swift | aCalledFunction() | s:F14swift_ide_test15aCalledFunctionFT_T_ | Ref,Call,RelCall | rel: 1
  // CHECK-NEXT: RelCall | aCaller() | s:F14swift_ide_test7aCallerFT_T_
}

// RelationChildOf, Implicit
struct AStruct {
  var x: Int
  // CHECK: [[@LINE-1]]:7 | instance-property/Swift | x | s:vV14swift_ide_test7AStruct1xSi | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AStruct | s:V14swift_ide_test7AStruct

  mutating func aMethod() {
    // CHECK: [[@LINE-1]]:17 | instance-method/Swift | aMethod() | s:FV14swift_ide_test7AStruct7aMethodFT_T_ | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:V14swift_ide_test7AStruct

    x += 1
    // CHECK: [[@LINE-1]]:5 | instance-property/Swift | x | s:vV14swift_ide_test7AStruct1xSi | Ref,Read,Writ | rel: 0
    // CHECK: [[@LINE-2]]:5 | function/acc-get/Swift | getter:x | s:FV14swift_ide_test7AStructg1xSi | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | aMethod() | s:FV14swift_ide_test7AStruct7aMethodFT_T_
    // CHECK-NEXT: RelRec | AStruct | s:V14swift_ide_test7AStruct
    // CHECK: [[@LINE-5]]:5 | function/acc-set/Swift | setter:x | s:FV14swift_ide_test7AStructs1xSi | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | aMethod() | s:FV14swift_ide_test7AStruct7aMethodFT_T_
    // CHECK-NEXT: RelRec | AStruct | s:V14swift_ide_test7AStruct
    // CHECK: [[@LINE-8]]:7 | function/infix-operator/Swift | +=(_:_:) | s:Fsoi2peFTRSiSi_T_ | Ref,Call,RelCall | rel: 1
    // CHECK-NEXT: RelCall | aMethod() | s:FV14swift_ide_test7AStruct7aMethodFT_T_
  }

  // RelationChildOf, RelationAccessorOf
  subscript(index: Int) -> Int {
    // CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:V14swift_ide_test7AStruct

    get {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-get/Swift | getter:subscript(_:) | s:FV14swift_ide_test7AStructg9subscriptFSiSi | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi

      return x
    }
    set {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-set/Swift | setter:subscript(_:) | s:FV14swift_ide_test7AStructs9subscriptFSiSi | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi

      x = newValue
    }
  }
}

class AClass {
  var y: AStruct;
  var z: [Int]

  init(x: Int) {
    y = AStruct(x: x)
    self.z = [1, 2, 3]
  }

  subscript(index: Int) -> Int {
    get {
      return z[0]
    }
    set {
      z[0] = newValue
    }
  }

  func foo() -> Int {
    return z[0]
  }
}

protocol AProtocol {
  // CHECK: [[@LINE-1]]:10 | protocol/Swift | AProtocol | [[AProtocol_USR:.*]] | Def | rel: 0
  func foo() -> Int
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | foo() | s:FP14swift_ide_test9AProtocol3fooFT_Si | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AProtocol | s:P14swift_ide_test9AProtocol
}

// RelationBaseOf, RelationOverrideOf
class ASubClass : AClass, AProtocol {
// CHECK: [[@LINE-1]]:7 | class/Swift | ASubClass | s:C14swift_ide_test9ASubClass | Def | rel: 0
// CHECK: [[@LINE-2]]:19 | class/Swift | AClass | s:C14swift_ide_test6AClass | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | ASubClass | s:C14swift_ide_test9ASubClass
// CHECK: [[@LINE-4]]:27 | protocol/Swift | AProtocol | s:P14swift_ide_test9AProtocol | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | ASubClass | s:C14swift_ide_test9ASubClass

  override func foo() -> Int {
    // CHECK: [[@LINE-1]]:17 | instance-method/Swift | foo() | s:FC14swift_ide_test9ASubClass3fooFT_Si | Def,RelChild,RelOver | rel: 3
    // CHECK-NEXT: RelOver | foo() | s:FC14swift_ide_test6AClass3fooFT_Si
    // CHECK-NEXT: RelOver | foo() | s:FP14swift_ide_test9AProtocol3fooFT_Si
    // CHECK-NEXT: RelChild | ASubClass | s:C14swift_ide_test9ASubClass
    return 1
  }
}

// RelationExtendedBy
extension AClass {
  // CHECK: [[@LINE-1]]:11 | extension/ext-class/Swift | AClass | [[EXT_ACLASS_USR:.*]] | Def | rel: 0
  // CHECK: [[@LINE-2]]:11 | class/Swift | AClass | s:C14swift_ide_test6AClass | Ref,RelExt | rel: 1
  // CHECK-NEXT: RelExt | AClass | [[EXT_ACLASS_USR]]

  func bar() -> Int { return 2 }
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | bar() | s:FC14swift_ide_test6AClass3barFT_Si | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass
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
  func foo() {}
}

var anInstance = AClass(x: 1)
// CHECK: [[@LINE-1]]:18 | class/Swift | AClass | s:C14swift_ide_test6AClass | Ref | rel: 0
// CHECK: [[@LINE-2]]:18 | constructor/Swift | init(x:) | s:FC14swift_ide_test6AClasscFT1xSi_S0_ | Ref,Call | rel: 0

anInstance.y.x = anInstance.y.x
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | y | s:vC14swift_ide_test6AClass1yVS_7AStruct | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:14 | instance-property/Swift | x | s:vV14swift_ide_test7AStruct1xSi | Ref,Writ | rel: 0
// CHECK: [[@LINE-4]]:18 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-5]]:29 | instance-property/Swift | y | s:vC14swift_ide_test6AClass1yVS_7AStruct | Ref,Read | rel: 0
// CHECK: [[@LINE-6]]:31 | instance-property/Swift | x | s:vV14swift_ide_test7AStruct1xSi | Ref,Read | rel: 0

anInstance.y.aMethod()
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | y | s:vC14swift_ide_test6AClass1yVS_7AStruct | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:14 | instance-method/Swift | aMethod() | s:FV14swift_ide_test7AStruct7aMethodFT_T_ | Ref,Call | rel: 0

// FIXME Write role of z occurrence on the RHS?
anInstance.z[1] = anInstance.z[0]
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | z | s:vC14swift_ide_test6AClass1zGSaSi_ | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:19 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-4]]:30 | instance-property/Swift | z | s:vC14swift_ide_test6AClass1zGSaSi_ | Ref,Read,Writ | rel: 0

let otherInstance = AStruct(x: 1)
// CHECK: [[@LINE-1]]:21 | struct/Swift | AStruct | s:V14swift_ide_test7AStruct | Ref | rel: 0

let _ = otherInstance[0]
// CHECK: [[@LINE-1]]:9 | variable/Swift | otherInstance | s:v14swift_ide_test13otherInstanceVS_7AStruct | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:22 | instance-property/subscript/Swift | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi | Ref,Read | rel: 0

let _ = anInstance[0]
// CHECK: [[@LINE-1]]:9 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:19 | instance-property/subscript/Swift | subscript(_:) | s:iC14swift_ide_test6AClass9subscriptFSiSi | Ref,Read | rel: 0

let aSubInstance: AClass = ASubClass(x: 1)
// CHECK: [[@LINE-1]]:5 | variable/Swift | aSubInstance | s:v14swift_ide_test12aSubInstanceCS_6AClass | Def | rel: 0
// CHECK: [[@LINE-2]]:28 | class/Swift | ASubClass | s:C14swift_ide_test9ASubClass | Ref | rel: 0

// Dynamic, RelationReceivedBy
let _ = aSubInstance.foo()
// CHECK: [[@LINE-1]]:9 | variable/Swift | aSubInstance | s:v14swift_ide_test12aSubInstanceCS_6AClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:22 | instance-method/Swift | foo() | s:FC14swift_ide_test6AClass3fooFT_Si | Ref,Call,Dyn,RelRec | rel: 1
// CHECK-NEXT: RelRec | AClass | s:C14swift_ide_test6AClass


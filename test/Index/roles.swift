// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// Definition
let x = 2
// CHECK: [[@LINE-1]]:5 | variable/Swift | x | s:v14swift_ide_test1xSi | Def | rel: 0

// Definition + Read of x
var y = x + 1
// CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:v14swift_ide_test1ySi | Def | rel: 0
// CHECK: [[@LINE-2]]:9 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:11 | infix-operator/Swift | +(_:_:) | s:Fsoi1pFTSiSi_Si | Ref,Call | rel: 0

// Read of x + Write of y
y = x + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:5 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0
// CHECK: [[@LINE-3]]:7 | infix-operator/Swift | +(_:_:) | s:Fsoi1pFTSiSi_Si | Ref,Call | rel: 0

// Read of y + Write of y
y += x
// CHECK: [[@LINE-1]]:1 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-2]]:3 | infix-operator/Swift | +=(_:_:) | s:Fsoi2peFTRSiSi_T_ | Ref,Call | rel: 0
// CHECK: [[@LINE-3]]:6 | variable/Swift | x | s:v14swift_ide_test1xSi | Ref,Read | rel: 0

var z: Int {
// CHECK: [[@LINE-1]]:5 | variable/Swift | z | s:v14swift_ide_test1zSi | Def | rel: 0
  get {
    // CHECK: [[@LINE-1]]:3 | accessor(get)/Swift | getter:z | s:F14swift_ide_testg1zSi | Def,RelChild,RelAcc | rel: 1

    return y
    // CHECK: [[@LINE-1]]:12 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Read | rel: 0
  }
  set {
    // CHECK: [[@LINE-1]]:3 | accessor(set)/Swift | setter:z | s:F14swift_ide_tests1zSi | Def,RelChild,RelAcc | rel: 1

    y = newValue
    // CHECK: [[@LINE-1]]:5 | variable/Swift | y | s:v14swift_ide_test1ySi | Ref,Writ | rel: 0
  }
}
// Write + Read of z
z = z + 1
// CHECK: [[@LINE-1]]:1 | variable/Swift | z | s:v14swift_ide_test1zSi | Ref,Writ | rel: 0
// CHECK: [[@LINE-2]]:1 | accessor(set)/Swift | setter:z | s:F14swift_ide_tests1zSi | Ref,Call,Impl,RelCall | rel: 1
// CHECK-NEXT: RelCall | z | s:v14swift_ide_test1zSi
// CHECK: [[@LINE-4]]:5 | variable/Swift | z | s:v14swift_ide_test1zSi | Ref,Read | rel: 0
// CHECK-NEXT: [[@LINE-5]]:5 | accessor(get)/Swift | getter:z | s:F14swift_ide_testg1zSi | Ref,Call,Impl,RelCall | rel: 1

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

struct aStruct {
  var x: Int
  mutating func aMethod() {
    x += 1
    // CHECK: [[@LINE-1]]:5 | instance-property/Swift | x | s:vV14swift_ide_test7aStruct1xSi | Ref,Read,Writ | rel: 0
    // CHECK: [[@LINE-2]]:5 | accessor(get)/Swift | getter:x | s:FV14swift_ide_test7aStructg1xSi | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | x | s:vV14swift_ide_test7aStruct1xSi
    // CHECK-NEXT: RelRec | aStruct | s:V14swift_ide_test7aStruct
    // CHECK: [[@LINE-5]]:5 | accessor(set)/Swift | setter:x | s:FV14swift_ide_test7aStructs1xSi | Ref,Call,Impl,RelRec,RelCall | rel: 2
    // CHECK-NEXT: RelCall | x | s:vV14swift_ide_test7aStruct1xSi
    // CHECK-NEXT: RelRec | aStruct | s:V14swift_ide_test7aStruct
    // CHECK: [[@LINE-8]]:7 | infix-operator/Swift | +=(_:_:) | s:Fsoi2peFTRSiSi_T_ | Ref,Call,RelCall | rel: 1
    // CHECK-NEXT: RelCall | aMethod() | s:FV14swift_ide_test7aStruct7aMethodFT_T_
  }
}

class aClass {
  var y: aStruct;

  init(x: Int) {
    y = aStruct(x: x)
  }
}

var anInstance = aClass(x: 1)
// CHECK: [[@LINE-1]]:18 | class/Swift | aClass | s:C14swift_ide_test6aClass | Ref | rel: 0
// CHECK: [[@LINE-2]]:18 | constructor/Swift | init(x:) | s:FC14swift_ide_test6aClasscFT1xSi_S0_ | Ref,Call | rel: 0

anInstance.y.aMethod()
// CHECK: [[@LINE-1]]:1 | variable/Swift | anInstance | s:v14swift_ide_test10anInstanceCS_6aClass | Ref,Read | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-property/Swift | y | s:vC14swift_ide_test6aClass1yVS_7aStruct | Ref,Read,Writ | rel: 0
// CHECK: [[@LINE-3]]:14 | instance-method/Swift | aMethod() | s:FV14swift_ide_test7aStruct7aMethodFT_T_ | Ref,Call | rel: 0
    
// Dynamic + Call
// Implicit

// ---- RELATIONS --------------------------------------------------------------

// RelationChildOf    
// RelationBaseOf    
// RelationOverrideOf
// RelationReceivedBy
// RelationCalledBy  
// RelationExtendedBy
// RelationAccessorOf

// ---- ERRONEOUS --------------------------------------------------------------

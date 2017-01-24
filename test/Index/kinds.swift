// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// Enum
enum AnEnumeration {
// CHECK: [[@LINE-1]]:6 | enum/Swift | AnEnumeration | s:O14swift_ide_test13AnEnumeration | Def | rel: 0

  // EnumElement
  case Element
  // CHECK: [[@LINE-1]]:8 | enumerator/Swift | Element | s:FO14swift_ide_test13AnEnumeration7ElementFMS0_S0_ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AnEnumeration | s:O14swift_ide_test13AnEnumeration
}

// Struct
struct AStruct {
  // CHECK: [[@LINE-1]]:8 | struct/Swift | AStruct | s:V14swift_ide_test7AStruct | Def | rel: 0
  
  var base: UnsafeMutablePointer<Int>

  // Subscript
  subscript(index: Int) -> Int {
    // CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:V14swift_ide_test7AStruct

    // Accessor + AccessorAddressor
    unsafeAddress {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-addr/Swift |  | s:FV14swift_ide_test7AStructlu9subscriptFSiSi | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi

      return UnsafePointer(base)
    }

    // Accessor + AccessorMutableAddressor
    unsafeMutableAddress {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-mutaddr/Swift |  | s:FV14swift_ide_test7AStructau9subscriptFSiSi | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:iV14swift_ide_test7AStruct9subscriptFSiSi

      return base
    }
  }
}

// Class
class AClass {
  // CHECK: [[@LINE-1]]:7 | class/Swift | AClass | s:C14swift_ide_test6AClass | Def | rel: 0

  // InstanceMethod
  func instanceMethod() {}
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | instanceMethod() | s:FC14swift_ide_test6AClass14instanceMethodFT_T_ | Def,RelChild | rel: 1
  // CHECK-NEXT:  RelChild | AClass | s:C14swift_ide_test6AClass

  // ClassMethod
  class func classMethod() {}
  // CHECK: [[@LINE-1]]:14 | class-method/Swift | classMethod() | s:ZFC14swift_ide_test6AClass11classMethodFT_T_ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

  // StaticMethod
  static func staticMethod() {}
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | s:ZFC14swift_ide_test6AClass12staticMethodFT_T_ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

  // InstanceProperty
  var instanceProperty: Int {
    // CHECK: [[@LINE-1]]:7 | instance-property/Swift | instanceProperty | s:vC14swift_ide_test6AClass16instancePropertySi | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

    // Accessor + AccessorGetter
    get {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-get/Swift | getter:instanceProperty | s:FC14swift_ide_test6AClassg16instancePropertySi | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | instanceProperty | s:vC14swift_ide_test6AClass16instancePropertySi

      return 1
    }

    // Accessor + AccessorSetter
    set {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-set/Swift | setter:instanceProperty | s:FC14swift_ide_test6AClasss16instancePropertySi | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | instanceProperty | s:vC14swift_ide_test6AClass16instancePropertySi
  }

  var observed = 0 {

    // Accessor + AccessorWillSet
    willSet {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-willset/Swift | willSet:observed | s:FC14swift_ide_test6AClassw8observedSi | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | observed | s:vC14swift_ide_test6AClass8observedSi

    // Accessor + AccessorDidSet
    didSet {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-didset/Swift | didSet:observed | s:FC14swift_ide_test6AClassW8observedSi | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | observed | s:vC14swift_ide_test6AClass8observedSi
  }

  // ClassProperty
  class let classProperty = 1
  // CHECK: [[@LINE-1]]:13 | class-property/Swift | classProperty | s:ZvC14swift_ide_test6AClass13classPropertySi | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

  // StaticProperty
  static let staticProperty = 1
  // CHECK: [[@LINE-1]]:14 | static-property/Swift | staticProperty | s:ZvC14swift_ide_test6AClass14staticPropertySi | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

  // Constructor
  init() {}
  // CHECK: [[@LINE-1]]:3 | constructor/Swift | init() | s:FC14swift_ide_test6AClasscFT_S0_ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass

  // Destructor
  deinit {}
  // CHECK: [[@LINE-1]]:3 | destructor/Swift | deinit | s:FC14swift_ide_test6AClassd | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:C14swift_ide_test6AClass
}

// Protocol
protocol AProtocol {
  // CHECK: [[@LINE-1]]:10 | protocol/Swift | AProtocol | s:P14swift_ide_test9AProtocol | Def | rel: 0

  // AssociatedType
  associatedtype T
  // CHECK: [[@LINE-1]]:18 | type-alias/associated-type/Swift | T | s:P14swift_ide_test9AProtocol1T | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AProtocol | s:P14swift_ide_test9AProtocol
}

// Extension
extension AnEnumeration {}
// CHECK: [[@LINE-1]]:11 | extension/ext-enum/Swift | AnEnumeration | s:O14swift_ide_test13AnEnumeration | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | enum/Swift | AnEnumeration | s:O14swift_ide_test13AnEnumeration | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AnEnumeration | s:O14swift_ide_test13AnEnumeration

extension AStruct {}
// CHECK: [[@LINE-1]]:11 | extension/ext-struct/Swift | AStruct | s:V14swift_ide_test7AStruct | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | struct/Swift | AStruct | s:V14swift_ide_test7AStruct | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AStruct | s:V14swift_ide_test7AStruct

extension AClass {}
// CHECK: [[@LINE-1]]:11 | extension/ext-class/Swift | AClass | s:C14swift_ide_test6AClass | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | class/Swift | AClass | s:C14swift_ide_test6AClass | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AClass | s:C14swift_ide_test6AClass

extension AProtocol {}
// CHECK: [[@LINE-1]]:11 | extension/ext-protocol/Swift | AProtocol | s:P14swift_ide_test9AProtocol | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | protocol/Swift | AProtocol | s:P14swift_ide_test9AProtocol | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AProtocol | s:P14swift_ide_test9AProtocol

// TypeAlias
typealias SomeAlias = AStruct
// CHECK: [[@LINE-1]]:11 | type-alias/Swift | SomeAlias | s:14swift_ide_test9SomeAlias | Def | rel: 0
// CHECK: [[@LINE-2]]:23 | struct/Swift | AStruct | s:V14swift_ide_test7AStruct | Ref | rel: 0

// GenericTypeParam
struct GenericStruct<ATypeParam> {}
// CHECK: [[@LINE-1]]:22 | type-alias/generic-type-param/Swift | ATypeParam | s:tV14swift_ide_test13GenericStruct10ATypeParamMx | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | GenericStruct | s:V14swift_ide_test13GenericStruct

func GenericFunc<ATypeParam>(_: ATypeParam) {}
// CHECK-NOT: [[@LINE-1]]:18 | type-alias/generic-type-param/Swift | ATypeParam | {{.*}} | Def,RelChild | rel: 1

// Function
func EmptyFunction() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | EmptyFunction() | s:F14swift_ide_test13EmptyFunctionFT_T_ | Def | rel: 0

// Variable
var foo = 1
// CHECK: [[@LINE-1]]:5 | variable/Swift | foo | s:v14swift_ide_test3fooSi | Def | rel: 0

// PrefixOperator
prefix func -(a: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:13 | function/prefix-operator/Swift | -(_:) | s:F14swift_ide_testop1sFVS_7AStructS0_ | Def | rel: 0

// PostfixOperator
postfix func ++(a: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:14 | function/postfix-operator/Swift | ++(_:) | s:F14swift_ide_testoP2ppFVS_7AStructS0_ | Def | rel: 0

// InfixOperator
func +(a: AStruct, b: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:6 | function/infix-operator/Swift | +(_:_:) | s:F14swift_ide_testoi1pFTVS_7AStructS0__S0_ | Def | rel: 0

class XCTestCase {}
class MyTestCase : XCTestCase {
  func testMe() {}
  // CHECK: [[@LINE-1]]:8 | instance-method(test)/Swift | testMe() |
  func testResult() -> Int? { return nil }
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | testResult() |
  func test(withInt: Int) {}
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | test(withInt:) |
}
class SubTestCase : MyTestCase {
  func testIt2() {}
  // CHECK: [[@LINE-1]]:8 | instance-method(test)/Swift | testIt2() |
}
extension SubTestCase {
  func testIt3() {}
  // CHECK: [[@LINE-1]]:8 | instance-method(test)/Swift | testIt3() |
}
class NonTestCase {
  func testMeNot() {}
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | testMeNot() |
}

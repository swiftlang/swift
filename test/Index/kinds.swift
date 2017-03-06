// RUN: %target-swift-ide-test -new-mangling-for-tests -print-indexed-symbols -source-filename %s | %FileCheck %s

// Enum
enum AnEnumeration {
// CHECK: [[@LINE-1]]:6 | enum/Swift | AnEnumeration | s:14swift_ide_test13AnEnumerationO | Def | rel: 0

  // EnumElement
  case Element
  // CHECK: [[@LINE-1]]:8 | enumerator/Swift | Element | s:14swift_ide_test13AnEnumerationO7ElementA2CmF | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AnEnumeration | s:14swift_ide_test13AnEnumerationO
}

// Struct
struct AStruct {
  // CHECK: [[@LINE-1]]:8 | struct/Swift | AStruct | s:14swift_ide_test7AStructV | Def | rel: 0
  
  var base: UnsafeMutablePointer<Int>

  // Subscript
  subscript(index: Int) -> Int {
    // CHECK: [[@LINE-1]]:3 | instance-property/subscript/Swift | subscript(_:) | s:14swift_ide_test7AStructV9subscriptS2ici | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AStruct | s:14swift_ide_test7AStructV

    // Accessor + AccessorAddressor
    unsafeAddress {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-addr/Swift |  | s:14swift_ide_test7AStructV9subscriptS2icflu | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:14swift_ide_test7AStructV9subscriptS2ici

      return UnsafePointer(base)
    }

    // Accessor + AccessorMutableAddressor
    unsafeMutableAddress {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-mutaddr/Swift |  | s:14swift_ide_test7AStructV9subscriptS2icfau | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | subscript(_:) | s:14swift_ide_test7AStructV9subscriptS2ici

      return base
    }
  }
}

// Class
class AClass {
  // CHECK: [[@LINE-1]]:7 | class/Swift | AClass | s:14swift_ide_test6AClassC | Def | rel: 0

  // InstanceMethod + Parameters
  func instanceMethod(a: Int, b b: Int, _ c: Int, d _: Int, _: Int) {}
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | instanceMethod(a:b:_:d:_:) | s:14swift_ide_test6AClassC14instanceMethodySi1a_Si1bS2i1dSitF | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC
  // CHECK: [[@LINE-3]]:23 | param/Swift | a | s:14swift_ide_test6AClassC14instanceMethodySi1a_Si1bS2i1dSitFAEL_Siv | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | instanceMethod(a:b:_:d:_:) | s:14swift_ide_test6AClassC14instanceMethodySi1a_Si1bS2i1dSitF
  // CHECK-NOT: [[@LINE-5]]:33 | param/Swift | b | s:{{.*}} | Def,RelChild | rel: 1
  // CHECK-NOT: [[@LINE-6]]:43 | param/Swift | c | s:{{.*}} | Def,RelChild | rel: 1
  // CHECK-NOT: [[@LINE-7]]:53 | param/Swift | d | s:{{.*}} | Def,RelChild | rel: 1
  // CHECK-NOT: [[@LINE-8]]:61 | param/Swift | _ | s:{{.*}} | Def,RelChild | rel: 1

  // ClassMethod
  class func classMethod() {}
  // CHECK: [[@LINE-1]]:14 | class-method/Swift | classMethod() | s:14swift_ide_test6AClassC11classMethodyyFZ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

  // StaticMethod
  static func staticMethod() {}
  // CHECK: [[@LINE-1]]:15 | static-method/Swift | staticMethod() | s:14swift_ide_test6AClassC12staticMethodyyFZ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

  // InstanceProperty
  var instanceProperty: Int {
    // CHECK: [[@LINE-1]]:7 | instance-property/Swift | instanceProperty | s:14swift_ide_test6AClassC16instancePropertySiv | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

    // Accessor + AccessorGetter
    get {
      // CHECK: [[@LINE-1]]:5 | instance-method/acc-get/Swift | getter:instanceProperty | s:14swift_ide_test6AClassC16instancePropertySifg | Def,RelChild,RelAcc | rel: 1
      // CHECK-NEXT: RelChild,RelAcc | instanceProperty | s:14swift_ide_test6AClassC16instancePropertySiv

      return 1
    }

    // Accessor + AccessorSetter
    set {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-set/Swift | setter:instanceProperty | s:14swift_ide_test6AClassC16instancePropertySifs | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | instanceProperty | s:14swift_ide_test6AClassC16instancePropertySiv
  }

  var observed = 0 {

    // Accessor + AccessorWillSet
    willSet {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-willset/Swift | willSet:observed | s:14swift_ide_test6AClassC8observedSifw | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | observed | s:14swift_ide_test6AClassC8observedSiv

    // Accessor + AccessorDidSet
    didSet {}
    // CHECK: [[@LINE-1]]:5 | instance-method/acc-didset/Swift | didSet:observed | s:14swift_ide_test6AClassC8observedSifW | Def,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | observed | s:14swift_ide_test6AClassC8observedSiv
  }

  // ClassProperty
  class let classProperty = 1
  // CHECK: [[@LINE-1]]:13 | class-property/Swift | classProperty | s:14swift_ide_test6AClassC13classPropertySivZ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

  // StaticProperty
  static let staticProperty = 1
  // CHECK: [[@LINE-1]]:14 | static-property/Swift | staticProperty | s:14swift_ide_test6AClassC14staticPropertySivZ | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

  // Constructor
  init() {}
  // CHECK: [[@LINE-1]]:3 | constructor/Swift | init() | s:14swift_ide_test6AClassCACycfc | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC

  // Destructor
  deinit {}
  // CHECK: [[@LINE-1]]:3 | destructor/Swift | deinit | s:14swift_ide_test6AClassCfd | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AClass | s:14swift_ide_test6AClassC
}

// Protocol
protocol AProtocol {
  // CHECK: [[@LINE-1]]:10 | protocol/Swift | AProtocol | s:14swift_ide_test9AProtocolP | Def | rel: 0

  // AssociatedType
  associatedtype T
  // CHECK: [[@LINE-1]]:18 | type-alias/associated-type/Swift | T | s:14swift_ide_test9AProtocolP1T | Def,RelChild | rel: 1
  // CHECK-NEXT: RelChild | AProtocol | s:14swift_ide_test9AProtocolP
}

// Extension
extension AnEnumeration { func extFn() {} }
// CHECK: [[@LINE-1]]:11 | extension/ext-enum/Swift | AnEnumeration | [[EXT_AnEnumeration_USR:s:e:s:14swift_ide_test13AnEnumerationO5extFnyyF]] | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | enum/Swift | AnEnumeration | s:14swift_ide_test13AnEnumerationO | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AnEnumeration | [[EXT_AnEnumeration_USR]]

extension AStruct { func extFn() {} }
// CHECK: [[@LINE-1]]:11 | extension/ext-struct/Swift | AStruct | [[EXT_AStruct_USR:s:e:s:14swift_ide_test7AStructV5extFnyyF]] | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | struct/Swift | AStruct | s:14swift_ide_test7AStructV | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AStruct | [[EXT_AStruct_USR]]

extension AClass { func extFn() {} }
// CHECK: [[@LINE-1]]:11 | extension/ext-class/Swift | AClass | [[EXT_AClass_USR:s:e:s:14swift_ide_test6AClassC5extFnyyF]] | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | class/Swift | AClass | s:14swift_ide_test6AClassC | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AClass | [[EXT_AClass_USR]]

extension AProtocol { func extFn() }
// CHECK: [[@LINE-1]]:11 | extension/ext-protocol/Swift | AProtocol | [[EXT_AProtocol_USR:s:e:s:14swift_ide_test9AProtocolPAAE5extFnyyF]] | Def | rel: 0
// CHECK: [[@LINE-2]]:11 | protocol/Swift | AProtocol | s:14swift_ide_test9AProtocolP | Ref,RelExt | rel: 1
// CHECK-NEXT: RelExt | AProtocol | [[EXT_AProtocol_USR]]

// TypeAlias
typealias SomeAlias = AStruct
// CHECK: [[@LINE-1]]:11 | type-alias/Swift | SomeAlias | s:14swift_ide_test9SomeAlias | Def | rel: 0
// CHECK: [[@LINE-2]]:23 | struct/Swift | AStruct | s:14swift_ide_test7AStructV | Ref | rel: 0

// GenericTypeParam
struct GenericStruct<ATypeParam> {}
// CHECK: [[@LINE-1]]:22 | type-alias/generic-type-param/Swift | ATypeParam | s:14swift_ide_test13GenericStructV10ATypeParamxmfp | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | GenericStruct | s:14swift_ide_test13GenericStructV

func GenericFunc<ATypeParam>(_: ATypeParam) {}
// CHECK-NOT: [[@LINE-1]]:18 | type-alias/generic-type-param/Swift | ATypeParam | {{.*}} | Def,RelChild | rel: 1

// Function
func EmptyFunction() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | EmptyFunction() | s:14swift_ide_test13EmptyFunctionyyF | Def | rel: 0

// Variable
var foo = 1
// CHECK: [[@LINE-1]]:5 | variable/Swift | foo | s:14swift_ide_test3fooSiv | Def | rel: 0

// PrefixOperator
prefix func -(a: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:13 | function/prefix-operator/Swift | -(_:) | s:14swift_ide_test1sopAA7AStructVADF | Def | rel: 0

// PostfixOperator
postfix func ++(a: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:14 | function/postfix-operator/Swift | ++(_:) | s:14swift_ide_test2ppoPAA7AStructVADF | Def | rel: 0

// InfixOperator
func +(a: AStruct, b: AStruct) -> AStruct { return a }
// CHECK: [[@LINE-1]]:6 | function/infix-operator/Swift | +(_:_:) | s:14swift_ide_test1poiAA7AStructVAD_ADtF | Def | rel: 0

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

@objc class TargetForIBAction {}
// CHECK: [[@LINE-1]]:13 | class/Swift | TargetForIBAction | [[TargetForIBAction_USR:.*]] | Def |
class AttrAnnots {
  @IBOutlet var iboutletString: AnyObject?
  // CHECK: [[@LINE-1]]:17 | instance-property(IB)/Swift | iboutletString |
  @IBAction func someibaction(o: TargetForIBAction) {}
  // CHECK: [[@LINE-1]]:18 | instance-method(IB)/Swift | someibaction(o:) | {{.*}} | Def,RelChild,RelIBType | rel: 2
  // CHECK-NEXT: RelIBType | TargetForIBAction | [[TargetForIBAction_USR]]
  @GKInspectable var gkString = "gk"
  // CHECK: [[@LINE-1]]:22 | instance-property(GKI)/Swift | gkString |
}

// CHECK: [[@LINE+1]]:7 | class/Swift | C1 | [[C1_USR:.*]] | Def | rel: 0
class C1 {}
// CHECK: [[@LINE+1]]:11 | type-alias/Swift | C1Alias | [[C1Alias_USR:.*]] | Def | rel: 0
typealias C1Alias = C1
// CHECK: [[@LINE+4]]:7 | class/Swift | SubC1 | [[SubC1_USR:.*]] | Def | rel: 0
// CHECK: [[@LINE+3]]:15 | type-alias/Swift | C1Alias | [[C1Alias_USR]] | Ref | rel: 0
// CHECK: [[@LINE+2]]:15 | class/Swift | C1 | [[C1_USR]] | Ref,Impl,RelBase | rel: 1
// CHECK-NEXT: RelBase | SubC1 | [[SubC1_USR]]
class SubC1 : C1Alias {}

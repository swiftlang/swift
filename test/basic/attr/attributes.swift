// RUN: %swift %s -verify


@unknown def f0() {} // expected-error{{unknown attribute 'unknown'}}

enum binary { 
  case Zero
  case One
  init() { self = .Zero }
}

@born_fragile
def f2() {}

@fragile
def f3() {}

@resilient
def f4() {}

@resilient, @fragile  // expected-error {{duplicate attribute}}
def f5() {}

@resilient @fragile   // expected-error {{duplicate attribute}}
def f5a() {}

var v0 : (@resilient binary) // expected-error {{attribute can only be applied to declarations, not types}}
var v1 : (@inout binary) // expected-error {{type '(@inout binary)' of variable is not materializable}}

@inout   // expected-error {{attribute can only be applied to types, not declarations}}
var v2 : binary

enum inout_carrier {
  case carry(@inout binary) // expected-error {{type of enum case is not materializable}}
}

def f5(x: @inout binary) {}
def f5(x: (y: @inout binary)) {} // FIXME: we need to diagnose non-materializable arguments in function types
def f6(_: (y: @inout binary)) {}

//===---
//===--- [class_protocol] attribute
//===---

@class_protocol // expected-error {{'class_protocol' attribute can only be applied to protocols}}
class NotProtocol {}
def fou(x: @class_protocol Int) {} // expected-error {{attribute can only be applied to declarations, not types}}

// [class_protocol] protocols can be [objc]
@objc @class_protocol
protocol ObjC1 {}
@class_protocol, @objc
protocol ObjC2 {}

@objc
protocol ObjCButNotClass {}  // expected-error{{only [class_protocol] protocols can be declared 'objc'}}

@class_protocol @objc
protocol ClassProtocolBase {}

@objc
protocol ClassProtocolEx : ClassProtocolBase {}

//===---
//===--- IB attributes
//===---

@ibaction  // expected-error {{only methods can be declared 'ibaction'}}
class IBActionClassTy {}

@ibaction // expected-error {{only methods can be declared 'ibaction'}}
struct IBActionStructTy {}

@ibaction // expected-error {{only methods can be declared 'ibaction'}}
def IBActionFunction() -> () {}

class IBActionWrapperTy {
  @ibaction
  def click() -> () {} // no-warning

  def outer() -> () {
    @ibaction  // expected-error {{only methods can be declared 'ibaction'}}
    def inner() -> () {}
  }
  @ibaction // expected-error {{only methods can be declared 'ibaction'}}
  var value : Void

  @ibaction
  def process(x: Int) -> Int {}  // expected-error {{methods declared 'ibaction' must return 'Void' (not 'Int')}}

  // @ibaction does /not/ semantically imply @objc.
  @ibaction, @ibaction  // expected-error {{duplicate attribute}}
  def doMagic() -> () {}

  @ibaction, @objc
  def moreMagic() -> () {} // no-warning
  @objc, @ibaction
  def evenMoreMagic() -> () {} // no-warning
}

@iboutlet // expected-error {{only properties can be declared 'iboutlet'}}
var iboutlet_global: Int

@iboutlet // expected-error {{only properties can be declared 'iboutlet'}}
class IBOutletClassTy {}
@iboutlet // expected-error {{only properties can be declared 'iboutlet'}}
struct IBStructTy {}

@iboutlet // expected-error {{only properties can be declared 'iboutlet'}}
def IBFunction() -> () {}

class IBOutletWrapperTy {
  @iboutlet
  var value : IBOutletWrapperTy // no-warning

  @iboutlet // expected-error {{only properties can be declared 'iboutlet'}}
  def click() -> () {}
}

@objc_block  // expected-error {{attribute can only be applied to types, not declarations}}
def foo() {}
def foo(x: @objc_block Int) {} // expected-error {{attribute only applies to syntactic function types}}
def foo(x: @objc_block (Int) -> Int) {}

@transparent
def zim() {}
@transparent
def zang()() {}
@transparent // expected-error{{'transparent' attribute is not supported for generic declarations}}
def zung<T>() {}
@transparent // expected-error{{'transparent' attribute cannot be applied to this declaration}}
var zippity : Int
def zoom(x: @transparent () -> ()) { } // expected-error{{attribute can only be applied to declarations, not types}}
protocol ProtoWithTransparent {
  @transparent// expected-error{{'transparent' attribute is not supported on declarations within protocols}}
  def transInProto()
}
class TestTranspClass : ProtoWithTransparent {
  @transparent  // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  init () {}
  @transparent // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  destructor () {}
  @transparent // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  static def transStatic() {}
  @transparent// expected-error{{'transparent' attribute is not supported on declarations within classes}}
  def transInProto() {}
}
struct TestTranspStruct : ProtoWithTransparent{
  @transparent
  init () {}
  @transparent
  init () {}
  @transparent// expected-error{{'transparent' attribute is not supported for generic declarations}}
  init <T> (x : T) { }
  @transparent
  static def transStatic() {}
  @transparent
  def transInProto() {}
}
@transparent // expected-error{{'transparent' attribute cannot be applied to this declaration}}
struct CannotHaveTransparentStruct {
  def m1() {}
}
@transparent // expected-error{{'transparent' attribute is only supported on struct and enum extensions}}
extension TestTranspClass {
  def tr1() {}
}
@transparent
extension TestTranspStruct {
  def tr1() {}
}
@transparent
extension binary {
  def tr1() {}
}

@cc   // expected-error {{attribute can only be applied to types, not declarations}}
def zim(x: Char) {}
def zim(x: @cc(cdecl) Int) {} // expected-error {{attribute only applies to syntactic function types}}
def zang(x: @cc(cdecl) (Int) -> Int) {}
def zung(x: @cc(cdecl), @objc_block (Int) -> Int) {}
def zippity(x: @cc(nevernude) (Int) -> Int) {} // expected-error {{unknown calling convention name 'nevernude'}}
def doo(x: @cc @objc_block (Int) -> Int) {} // expected-error {{expected '(' after 'cc' attribute}}
def dah(x: @cc() @objc_block (Int) -> Int) {} // expected-error {{expected calling convention name identifier in 'cc' attribute}}
def day(x: @cc(cdecl) @objc_block (Int) -> Int) {}

@thin  // expected-error {{attribute can only be applied to types, not declarations}}
def testThinDecl() -> () {}

@class_protocol
protocol Class {}
protocol NonClass {}

@objc
class Ty0 : Class, NonClass {
  init() { }
}

@weak
var weak0 : Ty0?

@weak, @unowned  // expected-error {{duplicate attribute}}
var weak1 : Ty0?
@weak, @weak      // expected-error {{duplicate attribute}}
var weak2 : Ty0?
@unowned
var weak3 : Ty0

@unowned @unowned  // expected-error {{duplicate attribute}}
var weak4 : Ty0
@unowned @weak // expected-error {{duplicate attribute}}
var weak5 : Ty0
@weak
var weak6 : Int // expected-error {{'weak' attribute cannot be applied to non-class type 'Int'}}
@unowned
var weak7 : Int // expected-error {{'unowned' attribute cannot be applied to non-class type 'Int'}}
@weak
var weak8 : Class? = Ty0()
@unowned
var weak9 : Class = Ty0()
@weak
var weak10 : NonClass = Ty0() // expected-error {{'weak' attribute cannot be applied to non-class type 'NonClass'; consider adding a class bound}}
@unowned
var weak11 : NonClass = Ty0() // expected-error {{'unowned' attribute cannot be applied to non-class type 'NonClass'; consider adding a class bound}}

@fragile @unowned
var weak12 : NonClass = Ty0() // expected-error {{'unowned' attribute cannot be applied to non-class type 'NonClass'; consider adding a class bound}}
@unowned @fragile
var weak13 : NonClass = Ty0() // expected-error {{'unowned' attribute cannot be applied to non-class type 'NonClass'; consider adding a class bound}}

@weak
var weak14 : Ty0 // expected-error {{'weak' variable should have Optional type 'Ty0?'}}
@weak
var weak15 : Class // expected-error {{'weak' variable should have Optional type 'Class?'}}

// Test typechecking of noreturn attribute.
@noreturn
def exit(_: Int) {}
def print(_: Int) {}
var f : (Int)->() = exit // no-error
var f2 = exit
f2 = print// expected-error {{expression does not type-check}}
exit(5) // no-error
@noreturn
def exit() -> () {}
@noreturn
def testFunctionOverload() -> () {
  exit()
}
def testRvalue(lhs: (), rhs: @noreturn ()->()) -> () {
  return rhs()
}

var fnr: @noreturn (_:Int)->() = exit
// This might be a desirable syntax, but it does not get properly propagated to SIL, so reject it for now.
@noreturn //expected-error {{attribute cannot be applied to declaration}}
var fpr: (_:Int)->() = exit

@exported // expected-error {{attribute cannot be applied to declaration}}
var exportVar : Int
@exported // expected-error {{attribute cannot be applied to declaration}}
def exportFunc() {}
@exported // expected-error {{attribute cannot be applied to declaration}}
struct ExportStruct {}

// Function result type attributes.
var func_result_type_attr : () -> @xyz Int  // expected-error {{unknown attribute 'xyz'}}

def func_result_attr() -> @xyz Int {       // expected-error {{unknown attribute 'xyz'}}
  return 4
}


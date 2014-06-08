// RUN: %swift %s -verify


@unknown func f0() {} // expected-error{{unknown attribute 'unknown'}}

enum binary { 
  case Zero
  case One
  init() { self = .Zero }
}

func f5(inout x: binary) {}

//===---
//===--- @class_protocol attribute
//===---

@class_protocol // expected-error {{'class_protocol' attribute can only be applied to protocols}}
class NotProtocol {}
func fou(x: @class_protocol Int) {} // expected-error {{attribute can only be applied to declarations, not types}}

// @class_protocol protocols can be @objc
@objc @class_protocol
protocol ObjC1 {}
@class_protocol @objc
protocol ObjC2 {}

@class_protocol // expected-note {{attribute already specified here}}
@class_protocol // expected-error {{duplicate attribute}}
protocol ClassProtocolDuplicate {}

@!class_protocol // expected-error {{attribute may not be inverted}}
protocol ClassProtocolInverted {}

@objc
protocol ObjCButNotClass {}

@class_protocol @objc
protocol ClassProtocolBase {}

@objc
protocol ClassProtocolEx : ClassProtocolBase {}

//===---
//===--- IB attributes
//===---

@IBDesignable
class IBDesignableClassTy {
  @IBDesignable func foo() {} // expected-error {{only classes can be declared 'IBDesignable'}}
}

@IBDesignable // expected-error {{only classes can be declared 'IBDesignable'}}
struct IBDesignableStructTy {}

@IBDesignable // expected-error {{only classes can be declared 'IBDesignable'}}
protocol IBDesignableProtTy {}

class Inspect {
  @IBInspectable var value : Int = 0
  @IBInspectable func foo() {} // expected-error {{only instance properties can be declared 'IBInspectable'}}

  @IBInspectable class var cval: Int { return 0 } // expected-error {{only instance properties can be declared 'IBInspectable'}}
}
@IBInspectable var ibinspectable_global : Int // expected-error {{only instance properties can be declared 'IBInspectable'}}


@objc_block  // expected-error {{attribute can only be applied to types, not declarations}}
func foo() {}
func foo(x: @objc_block Int) {} // expected-error {{attribute only applies to syntactic function types}}
func foo(x: @objc_block (Int) -> Int) {}

@transparent
func zim() {}
@transparent
func zang()() {}
@transparent
func zung<T>() {}
@transparent // expected-error{{'transparent' attribute cannot be applied to this declaration}}
var zippity : Int
func zoom(x: @transparent () -> ()) { } // expected-error{{attribute can only be applied to declarations, not types}}
protocol ProtoWithTransparent {
  @transparent// expected-error{{'transparent' attribute is not supported on declarations within protocols}}
  func transInProto()
}
class TestTranspClass : ProtoWithTransparent {
  @transparent  // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  init () {}
  @transparent // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  deinit {}
  @transparent // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  class func transStatic() {}
  @transparent// expected-error{{'transparent' attribute is not supported on declarations within classes}}
  func transInProto() {}
}
struct TestTranspStruct : ProtoWithTransparent{
  @transparent
  init () {}
  @transparent
  init <T> (x : T) { }
  @transparent
  static func transStatic() {}
  @transparent
  func transInProto() {}
}
@transparent // expected-error{{'transparent' attribute cannot be applied to this declaration}}
struct CannotHaveTransparentStruct {
  func m1() {}
}
@transparent // expected-error{{'transparent' attribute is only supported on struct and enum extensions}}
extension TestTranspClass {
  func tr1() {}
}
@transparent
extension TestTranspStruct {
  func tr1() {}
}
@transparent
extension binary {
  func tr1() {}
}

class transparentOnCalssVar {
  @transparent var max: Int { return 0xFF }; // expected-error {{'transparent' attribute is not supported on declarations within classes}}
  func blah () {
    var x: Int = max
  }
};

class transparentOnCalssVar2 {
  var max: Int {
    @transparent // expected-error {{'transparent' attribute is not supported on declarations within classes}}
    get {
      return 0xFF
    }
  }
  func blah () {
    var x: Int = max
  }
};

@cc   // expected-error {{attribute can only be applied to types, not declarations}}
func zim(x: UnicodeScalar) {}

@thin  // expected-error {{attribute can only be applied to types, not declarations}}
func testThinDecl() -> () {}

@class_protocol
protocol Class {}
protocol NonClass {}

@objc
class Ty0 : Class, NonClass {
  init() { }
}

weak
var weak0 : Ty0?

weak
var weak0x : Ty0?

weak unowned var weak1 : Ty0? // expected-error {{declaration cannot have multiple ownership specifiers}}
weak weak var weak2 : Ty0? // expected-error {{declaration cannot have multiple ownership specifiers}}
unowned var weak3 : Ty0

unowned var weak3a : Ty0
unowned(safe) var weak3b : Ty0
unowned(unsafe) var weak3c : Ty0


unowned unowned var weak4 : Ty0  // expected-error {{declaration cannot have multiple ownership specifiers}}

unowned weak var weak5 : Ty0 // expected-error {{declaration cannot have multiple ownership specifiers}}

strong weak var weak5a : Ty0  // expected-error {{declaration cannot have multiple ownership specifiers}}

weak
var weak6 : Int // expected-error {{'weak' cannot be applied to non-class type 'Int'}}
unowned
var weak7 : Int // expected-error {{'unowned' cannot be applied to non-class type 'Int'}}
weak
var weak8 : Class? = Ty0()
unowned var weak9 : Class = Ty0()
weak
var weak10 : NonClass = Ty0() // expected-error {{'weak' cannot be applied to non-class type 'NonClass'; consider adding a class bound}}
unowned
var weak11 : NonClass = Ty0() // expected-error {{'unowned' cannot be applied to non-class type 'NonClass'; consider adding a class bound}}

unowned
var weak12 : NonClass = Ty0() // expected-error {{'unowned' cannot be applied to non-class type 'NonClass'; consider adding a class bound}}
unowned
var weak13 : NonClass = Ty0() // expected-error {{'unowned' cannot be applied to non-class type 'NonClass'; consider adding a class bound}}

weak
var weak14 : Ty0 // expected-error {{'weak' variable should have optional type 'Ty0?'}}
weak
var weak15 : Class // expected-error {{'weak' variable should have optional type 'Class?'}}

weak var weak16 : Class!

@weak var weak17 : Class? // expected-error {{'@weak' is not an attribute, use the 'weak' keyword instead}}


@exported var exportVar: Int // expected-error {{'exported' attribute cannot be applied to this declaration}}{{1-10=}}
@exported func exportFunc() {} // expected-error {{'exported' attribute cannot be applied to this declaration}}{{1-10=}}
@exported struct ExportStruct {} // expected-error {{'exported' attribute cannot be applied to this declaration}}{{1-10=}}


// Function result type attributes.
var func_result_type_attr : () -> @xyz Int  // expected-error {{unknown attribute 'xyz'}}

func func_result_attr() -> @xyz Int {       // expected-error {{unknown attribute 'xyz'}}
  return 4
}

// @thin and @cc are only accepted in SIL.
var thinFunc : @thin () -> () // expected-error {{attribute is not supported}}
var ccFunc : @cc(cdecl) () -> () // expected-error {{attribute is not supported}}

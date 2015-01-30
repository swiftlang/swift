// RUN: %target-parse-verify-swift

@unknown func f0() {} // expected-error{{unknown attribute 'unknown'}}

enum binary { 
  case Zero
  case One
  init() { self = .Zero }
}

func f5(inout x: binary) {}

//===---
//===--- IB attributes
//===---

@IBDesignable
class IBDesignableClassTy {
  @IBDesignable func foo() {} // expected-error {{'IBDesignable' attribute cannot be applied to this declaration}}
}

@IBDesignable // expected-error {{'IBDesignable' attribute cannot be applied to this declaration}}
struct IBDesignableStructTy {}

@IBDesignable // expected-error {{'IBDesignable' attribute cannot be applied to this declaration}}
protocol IBDesignableProtTy {}

@IBDesignable // expected-error {{'IBDesignable' attribute can only be applied to classes and extensions of classes}}
extension IBDesignableStructTy {}

class IBDesignableClassExtensionTy {}
@IBDesignable // okay
extension IBDesignableClassExtensionTy {}

class Inspect {
  @IBInspectable var value : Int = 0
  @IBInspectable func foo() {} // expected-error {{'IBInspectable' may only be used on 'var' declarations}}

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
@transparent // expected-error{{'transparent' attribute cannot be applied to stored properties}}
var zippity : Int
func zoom(x: @transparent () -> ()) { } // expected-error{{attribute can only be applied to declarations, not types}}
protocol ProtoWithTransparent {
  @transparent// expected-error{{'transparent' attribute is not supported on declarations within protocols}}
  func transInProto()
}
class TestTranspClass : ProtoWithTransparent {
  @transparent  // expected-error{{'transparent' attribute is not supported on declarations within classes}}
  init () {}
  @transparent // expected-error{{'transparent' attribute cannot be applied to this declaration}}
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

protocol Class : class {}
protocol NonClass {}

@objc
class Ty0 : Class, NonClass {
  init() { }
}

// Attributes that should be reported by parser as unknown
// See rdar://19533915
@__accessibility struct S__accessibility {} // expected-error{{unknown attribute '__accessibility'}}
@__raw_doc_comment struct S__raw_doc_comment {} // expected-error{{unknown attribute '__raw_doc_comment'}}
@__objc_bridged struct S__objc_bridged {} // expected-error{{unknown attribute '__objc_bridged'}}

weak
var weak0 : Ty0?

weak
var weak0x : Ty0?

weak unowned var weak1 : Ty0? // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
weak weak var weak2 : Ty0? // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
unowned var weak3 : Ty0

unowned var weak3a : Ty0
unowned(safe) var weak3b : Ty0
unowned(unsafe) var weak3c : Ty0


unowned unowned var weak4 : Ty0  // expected-error {{duplicate modifier}}  expected-note {{modifier already specified here}}

unowned weak var weak5 : Ty0 // expected-error {{duplicate modifier}}  expected-note {{modifier already specified here}}

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

@weak var weak17 : Class? // expected-error {{'weak' is a declaration modifier, not an attribute}}


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

@inline(never) func nolineFunc() {}
@inline(never) var noinlineVar : Int // expected-error {{'inline(never)' attribute cannot be applied to this declaration}}
@inline(never) class FooClass { // expected-error {{'inline(never)' attribute cannot be applied to this declaration}}
}

@inline(__always) func AlwaysInlineFunc() {}
@inline(__always) var alwaysInlineVar : Int // expected-error {{'inline(__always)' attribute cannot be applied to this declaration}}
@inline(__always) class FooClass2 { // expected-error {{'inline(__always)' attribute cannot be applied to this declaration}}
}

class A {
  @inline(never) init(a : Int) {}
  var b : Int {
    @inline(never) get {
      return 42
    }
    @inline(never) set {
    }
  }
}

class B {
  @inline(__always) init(a : Int) {}
  var b : Int {
    @inline(__always) get {
      return 42
    }
    @inline(__always) set {
    }
  }
}

class SILStored {
  @sil_stored var x : Int = 42  // expected-error {{'sil_stored' only allowed in SIL modules}}
}

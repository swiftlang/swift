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
  @IBDesignable func foo() {} // expected-error {{@IBDesignable cannot be applied to this declaration}} {{3-17=}}
}

@IBDesignable // expected-error {{@IBDesignable cannot be applied to this declaration}} {{1-15=}}
struct IBDesignableStructTy {}

@IBDesignable // expected-error {{@IBDesignable cannot be applied to this declaration}} {{1-15=}}
protocol IBDesignableProtTy {}

@IBDesignable // expected-error {{@IBDesignable can only be applied to classes and extensions of classes}} {{1-15=}}
extension IBDesignableStructTy {}

class IBDesignableClassExtensionTy {}
@IBDesignable // okay
extension IBDesignableClassExtensionTy {}

class Inspect {
  @IBInspectable var value : Int = 0
  @IBInspectable func foo() {} // expected-error {{@IBInspectable may only be used on 'var' declarations}} {{3-18=}}

  @IBInspectable class var cval: Int { return 0 } // expected-error {{only instance properties can be declared @IBInspectable}} {{3-18=}}
}
@IBInspectable var ibinspectable_global : Int // expected-error {{only instance properties can be declared @IBInspectable}} {{1-16=}}


@objc_block  // expected-error {{attribute can only be applied to types, not declarations}}
func foo() {}
func foo(x: @convention(block) Int) {} // expected-error {{attribute only applies to syntactic function types}}
func foo(x: @convention(block) (Int) -> Int) {}

@transparent
func zim() {}
@transparent
func zang()() {} // expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
@transparent
func zung<T>(_: T) {}
@transparent // expected-error{{@transparent cannot be applied to stored properties}} {{1-14=}}
var zippity : Int
func zoom(x: @transparent () -> ()) { } // expected-error{{attribute can only be applied to declarations, not types}} {{1-1=@transparent }} {{14-27=}}
protocol ProtoWithTransparent {
  @transparent// expected-error{{@transparent is not supported on declarations within protocols}} {{3-15=}}
  func transInProto()
}
class TestTranspClass : ProtoWithTransparent {
  @transparent  // expected-error{{@transparent is not supported on declarations within classes}} {{3-16=}}
  init () {}
  @transparent // expected-error{{@transparent cannot be applied to this declaration}} {{3-16=}}
  deinit {}
  @transparent // expected-error{{@transparent is not supported on declarations within classes}} {{3-16=}}
  class func transStatic() {}
  @transparent// expected-error{{@transparent is not supported on declarations within classes}} {{3-15=}}
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
@transparent // expected-error{{@transparent cannot be applied to this declaration}} {{1-14=}}
struct CannotHaveTransparentStruct {
  func m1() {}
}
@transparent // expected-error{{@transparent is only supported on struct and enum extensions}} {{1-14=}}
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
  @transparent var max: Int { return 0xFF }; // expected-error {{@transparent is not supported on declarations within classes}} {{3-16=}}
  func blah () {
    var _: Int = max
  }
};

class transparentOnCalssVar2 {
  var max: Int {
    @transparent // expected-error {{@transparent is not supported on declarations within classes}} {{5-18=}}
    get {
      return 0xFF
    }
  }
  func blah () {
    var _: Int = max
  }
};

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

@weak var weak17 : Class? // expected-error {{'weak' is a declaration modifier, not an attribute}} {{1-2=}}


@exported var exportVar: Int // expected-error {{@exported may only be used on 'import' declarations}}{{1-11=}}
@exported func exportFunc() {} // expected-error {{@exported may only be used on 'import' declarations}}{{1-11=}}
@exported struct ExportStruct {} // expected-error {{@exported may only be used on 'import' declarations}}{{1-11=}}


// Function result type attributes.
var func_result_type_attr : () -> @xyz Int  // expected-error {{unknown attribute 'xyz'}}

func func_result_attr() -> @xyz Int {       // expected-error {{unknown attribute 'xyz'}}
  return 4
}

// @thin is not supported except in SIL.
var thinFunc : @thin () -> () // expected-error {{attribute is not supported}}

@inline(never) func nolineFunc() {}
@inline(never) var noinlineVar : Int // expected-error {{@inline(never) cannot be applied to this declaration}} {{1-16=}}
@inline(never) class FooClass { // expected-error {{@inline(never) cannot be applied to this declaration}} {{1-16=}}
}

@inline(__always) func AlwaysInlineFunc() {}
@inline(__always) var alwaysInlineVar : Int // expected-error {{@inline(__always) cannot be applied to this declaration}} {{1-19=}}
@inline(__always) class FooClass2 { // expected-error {{@inline(__always) cannot be applied to this declaration}} {{1-19=}}
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

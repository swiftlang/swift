// RUN: %target-typecheck-verify-swift

@unknown func f0() {} // expected-error{{unknown attribute 'unknown'}}
@unknown(x,y) func f1() {} // expected-error{{unknown attribute 'unknown'}}

enum binary { 
  case Zero
  case One
  init() { self = .Zero }
}

func f5(x: inout binary) {}

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
  @IBInspectable var value : Int = 0 // okay
  @GKInspectable var value2: Int = 0 // okay

  @IBInspectable func foo() {} // expected-error {{@IBInspectable may only be used on 'var' declarations}} {{3-18=}}
  @GKInspectable func foo2() {} // expected-error {{@GKInspectable may only be used on 'var' declarations}} {{3-18=}}

  @IBInspectable class var cval: Int { return 0 } // expected-error {{only instance properties can be declared @IBInspectable}} {{3-18=}}
  @GKInspectable class var cval2: Int { return 0 } // expected-error {{only instance properties can be declared @GKInspectable}} {{3-18=}}
}
@IBInspectable var ibinspectable_global : Int // expected-error {{only instance properties can be declared @IBInspectable}} {{1-16=}}
@GKInspectable var gkinspectable_global : Int // expected-error {{only instance properties can be declared @GKInspectable}} {{1-16=}}


func foo(x: @convention(block) Int) {} // expected-error {{@convention attribute only applies to function types}}
func foo(x: @convention(block) (Int) -> Int) {}

@_transparent
func zim() {}
@_transparent
func zung<T>(_: T) {}
@_transparent // expected-error{{@_transparent cannot be applied to stored properties}} {{1-15=}}
var zippity : Int
func zoom(x: @_transparent () -> ()) { } // expected-error{{attribute can only be applied to declarations, not types}} {{1-1=@_transparent }} {{14-28=}}
protocol ProtoWithTransparent {
  @_transparent// expected-error{{@_transparent is not supported on declarations within protocols}} {{3-16=}}
  func transInProto()
}
class TestTranspClass : ProtoWithTransparent {
  @_transparent  // expected-error{{@_transparent is not supported on declarations within classes}} {{3-17=}}
  init () {}
  @_transparent // expected-error{{@_transparent cannot be applied to this declaration}} {{3-17=}}
  deinit {}
  @_transparent // expected-error{{@_transparent is not supported on declarations within classes}} {{3-17=}}
  class func transStatic() {}
  @_transparent// expected-error{{@_transparent is not supported on declarations within classes}} {{3-16=}}
  func transInProto() {}
}
struct TestTranspStruct : ProtoWithTransparent{
  @_transparent
  init () {}
  @_transparent
  init <T> (x : T) { }
  @_transparent
  static func transStatic() {}
  @_transparent
  func transInProto() {}
}
@_transparent // expected-error{{@_transparent cannot be applied to this declaration}} {{1-15=}}
struct CannotHaveTransparentStruct {
  func m1() {}
}
@_transparent // expected-error{{@_transparent cannot be applied to this declaration}} {{1-15=}}
extension TestTranspClass {
  func tr1() {}
}
@_transparent // expected-error{{@_transparent cannot be applied to this declaration}} {{1-15=}}
extension TestTranspStruct {
  func tr1() {}
}
@_transparent // expected-error{{@_transparent cannot be applied to this declaration}} {{1-15=}}
extension binary {
  func tr1() {}
}

class transparentOnClassVar {
  @_transparent var max: Int { return 0xFF }; // expected-error {{@_transparent is not supported on declarations within classes}} {{3-17=}}
  func blah () {
    var _: Int = max
  }
};

class transparentOnClassVar2 {
  var max: Int {
    @_transparent // expected-error {{@_transparent is not supported on declarations within classes}} {{5-19=}}
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
var weak6 : Int // expected-error {{'weak' may only be applied to class and class-bound protocol types, not 'Int'}}
unowned
var weak7 : Int // expected-error {{'unowned' may only be applied to class and class-bound protocol types, not 'Int'}}
weak
var weak8 : Class? = Ty0()
unowned var weak9 : Class = Ty0()
weak
var weak10 : NonClass = Ty0() // expected-error {{'weak' may not be applied to non-class-bound 'NonClass'; consider adding a protocol conformance that has a class bound}}
unowned
var weak11 : NonClass = Ty0() // expected-error {{'unowned' may not be applied to non-class-bound 'NonClass'; consider adding a protocol conformance that has a class bound}}

unowned
var weak12 : NonClass = Ty0() // expected-error {{'unowned' may not be applied to non-class-bound 'NonClass'; consider adding a protocol conformance that has a class bound}}
unowned
var weak13 : NonClass = Ty0() // expected-error {{'unowned' may not be applied to non-class-bound 'NonClass'; consider adding a protocol conformance that has a class bound}}

weak
var weak14 : Ty0 // expected-error {{'weak' variable should have optional type 'Ty0?'}}
weak
var weak15 : Class // expected-error {{'weak' variable should have optional type 'Class?'}}

weak var weak16 : Class!

@weak var weak17 : Class? // expected-error {{'weak' is a declaration modifier, not an attribute}} {{1-2=}}


@_exported var exportVar: Int // expected-error {{@_exported may only be used on 'import' declarations}}{{1-12=}}
@_exported func exportFunc() {} // expected-error {{@_exported may only be used on 'import' declarations}}{{1-12=}}
@_exported struct ExportStruct {} // expected-error {{@_exported may only be used on 'import' declarations}}{{1-12=}}


// Function result type attributes.
var func_result_type_attr : () -> @xyz Int  // expected-error {{unknown attribute 'xyz'}}

func func_result_attr() -> @xyz Int {       // expected-error {{unknown attribute 'xyz'}}
  return 4
}

func func_with_unknown_attr1(@unknown(*) x: Int) {} // expected-error {{unknown attribute 'unknown'}}
func func_with_unknown_attr2(x: @unknown(_) Int) {} // expected-error {{unknown attribute 'unknown'}}
func func_with_unknown_attr3(x: @unknown(Int) -> Int) {} // expected-error {{unknown attribute 'unknown'}}
func func_with_unknown_attr4(x: @unknown(Int) throws -> Int) {} // expected-error {{unknown attribute 'unknown'}}
func func_with_unknown_attr5(x: @unknown (x: Int, y: Int)) {} // expected-error {{unknown attribute 'unknown'}}
func func_with_unknown_attr6(x: @unknown(x: Int, y: Int)) {} // expected-error {{unknown attribute 'unknown'}} expected-error {{expected parameter type following ':'}}
func func_with_unknown_attr7(x: @unknown (Int) () -> Int) {} // expected-error {{unknown attribute 'unknown'}} expected-error {{expected ',' separator}} {{47-47=,}} expected-error {{unnamed parameters must be written with the empty name '_'}} {{48-48=_: }}

func func_type_attribute_with_space(x: @convention (c) () -> Int) {} // OK. Known attributes can have space before its paren.

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

@_show_in_interface protocol _underscored {}
@_show_in_interface class _notapplicable {} // expected-error {{may only be used on 'protocol' declarations}}

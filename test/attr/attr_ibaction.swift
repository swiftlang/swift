// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}}
var iboutlet_global: Int

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}}
class IBOutletClassTy {}
@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}}
struct IBStructTy {}

@IBAction // expected-error {{only instance methods can be declared @IBAction}}
func IBFunction() -> () {}

class IBActionWrapperTy {
  @IBAction
  func click(_: AnyObject) -> () {} // no-warning

  func outer(_: AnyObject) -> () {
    @IBAction  // expected-error {{only instance methods can be declared @IBAction}}
    func inner(_: AnyObject) -> () {}
  }
  @IBAction // expected-error {{@IBAction may only be used on 'func' declarations}}
  var value : Void = ()

  @IBAction
  func process(x: AnyObject) -> Int {}  // expected-error {{methods declared @IBAction must return 'Void' (not 'Int')}}

  // @IBAction does /not/ semantically imply @objc.
  @IBAction // expected-note {{attribute already specified here}}
  @IBAction // expected-error {{duplicate attribute}}
  func doMagic(_: AnyObject) -> () {}

  @IBAction @objc
  func moreMagic(_: AnyObject) -> () {} // no-warning
  @objc @IBAction
  func evenMoreMagic(_: AnyObject) -> () {} // no-warning
}

struct S { }
enum E { }

protocol P1 { }
protocol P2 { }

protocol CP1 : class { }
protocol CP2 : class { }

@objc protocol OP1 { }
@objc protocol OP2 { }


// Check which argument types @IBAction can take.
@objc class X {
  // Class type
  @IBAction func action1(_: X) {}
  @IBAction func action2(_: X?) {}
  @IBAction func action3(_: X!) {}

  // AnyObject
  @IBAction func action4(_: AnyObject) {}
  @IBAction func action5(_: AnyObject?) {}
  @IBAction func action6(_: AnyObject!) {}

  // Protocol types
  @IBAction func action7(_: P1) {} // expected-error{{argument to @IBAction method cannot have non-object type 'P1'}}
  @IBAction func action8(_: CP1) {} // expected-error{{argument to @IBAction method cannot have non-object type 'CP1'}}
  @IBAction func action9(_: OP1) {}
  @IBAction func action10(_: P1?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action11(_: CP1?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action12(_: OP1?) {}
  @IBAction func action13(_: P1!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action14(_: CP1!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action15(_: OP1!) {}

  // Class metatype
  @IBAction func action15b(_: X.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action16(_: X.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action17(_: X.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // AnyClass
  @IBAction func action18(_: AnyClass) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action19(_: AnyClass?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action20(_: AnyClass!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // Protocol types
  @IBAction func action21(_: P1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action22(_: CP1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action23(_: OP1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action24(_: P1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action25(_: CP1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action26(_: OP1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action27(_: P1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action28(_: CP1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action29(_: OP1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // Other bad cases
  @IBAction func action30(_: S) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action31(_: E) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  init() { }
}

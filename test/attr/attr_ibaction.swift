// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
var iboutlet_global: Int

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
class IBOutletClassTy {}
@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
struct IBStructTy {}

@IBAction // expected-error {{only instance methods can be declared @IBAction}} {{1-11=}}
func IBFunction() -> () {}

class IBActionWrapperTy {
  @IBAction
  func click(_ _: AnyObject) -> () {} // no-warning

  func outer(_ _: AnyObject) -> () {
    @IBAction  // expected-error {{only instance methods can be declared @IBAction}} {{5-15=}}
    func inner(_ _: AnyObject) -> () {}
  }
  @IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{3-13=}}
  var value : Void = ()

  @IBAction
  func process(_ x: AnyObject) -> Int {}  // expected-error {{methods declared @IBAction must return 'Void' (not 'Int')}}

  // @IBAction does /not/ semantically imply @objc.
  @IBAction // expected-note {{attribute already specified here}}
  @IBAction // expected-error {{duplicate attribute}}
  func doMagic(_ _: AnyObject) -> () {}

  @IBAction @objc
  func moreMagic(_ _: AnyObject) -> () {} // no-warning
  @objc @IBAction
  func evenMoreMagic(_ _: AnyObject) -> () {} // no-warning
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
  @IBAction func action1(_ _: X) {}
  @IBAction func action2(_ _: X?) {}
  @IBAction func action3(_ _: X!) {}

  // AnyObject
  @IBAction func action4(_ _: AnyObject) {}
  @IBAction func action5(_ _: AnyObject?) {}
  @IBAction func action6(_ _: AnyObject!) {}

  // Protocol types
  @IBAction func action7(_ _: P1) {} // expected-error{{argument to @IBAction method cannot have non-object type 'P1'}}
  @IBAction func action8(_ _: CP1) {} // expected-error{{argument to @IBAction method cannot have non-object type 'CP1'}}
  @IBAction func action9(_ _: OP1) {}
  @IBAction func action10(_ _: P1?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action11(_ _: CP1?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action12(_ _: OP1?) {}
  @IBAction func action13(_ _: P1!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action14(_ _: CP1!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action15(_ _: OP1!) {}

  // Class metatype
  @IBAction func action15b(_ _: X.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action16(_ _: X.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action17(_ _: X.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // AnyClass
  @IBAction func action18(_ _: AnyClass) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action19(_ _: AnyClass?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action20(_ _: AnyClass!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // Protocol types
  @IBAction func action21(_ _: P1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action22(_ _: CP1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action23(_ _: OP1.Type) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action24(_ _: P1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action25(_ _: CP1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action26(_ _: OP1.Type?) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action27(_ _: P1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action28(_ _: CP1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action29(_ _: OP1.Type!) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  // Other bad cases
  @IBAction func action30(_ _: S) {} // expected-error{{argument to @IBAction method cannot have non-object type}}
  @IBAction func action31(_ _: E) {} // expected-error{{argument to @IBAction method cannot have non-object type}}

  init() { }
}

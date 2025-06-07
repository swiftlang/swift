// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
var iboutlet_global: Int

var iboutlet_accessor: Int {
  @IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{3-13=}}
  get { return 42 }
}

@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
class IBOutletClassTy {}
@IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{1-11=}}
struct IBStructTy {}

@IBAction // expected-error {{only instance methods can be declared @IBAction}} {{1-11=}}
func IBFunction() -> () {}

class IBActionWrapperTy {
  @IBAction
  func click(_: AnyObject) -> () {} // no-warning

  func outer(_: AnyObject) -> () {
    @IBAction  // expected-error {{only instance methods can be declared @IBAction}} {{5-15=}}
    func inner(_: AnyObject) -> () {}
  }
  @IBAction // expected-error {{@IBAction may only be used on 'func' declarations}} {{3-13=}}
  var value : Void = ()

  @IBAction
  func process(x: AnyObject) -> Int {}  // expected-error {{methods declared @IBAction must not return a value}}

  // @IBAction does /not/ semantically imply @objc.
  @IBAction // expected-note {{attribute already specified here}}
  @IBAction // expected-error {{duplicate attribute}}
  func doMagic(_: AnyObject) -> () {}

  @IBAction @objc
  func moreMagic(_: AnyObject) -> () {} // no-warning
  @objc @IBAction
  func evenMoreMagic(_: AnyObject) -> () {} // no-warning

  @available(SwiftStdlib 5.5, *)
  @IBAction
  func asyncIBActionNoSpace(_: AnyObject) async -> () {}
  // expected-error@-1 {{@IBAction instance method cannot be async}}
  // expected-note@-2 {{remove 'async' and wrap in 'Task' to use concurrency in 'asyncIBActionNoSpace'}}{{45:3-47:57=@available(SwiftStdlib 5.5, *)\n  @IBAction\n  func asyncIBActionNoSpace(_: AnyObject) -> () {\nTask { @MainActor in \}\n\}}}

  @available(SwiftStdlib 5.5, *)
  @IBAction
  func asyncIBActionWithFullBody(_: AnyObject) async {
      print("Hello World")
  }
  // expected-error@-3 {{@IBAction instance method cannot be async}}
  // expected-note@-4 {{remove 'async' and wrap in 'Task' to use concurrency in 'asyncIBActionWithFullBody'}}{{51:3-55:4=@available(SwiftStdlib 5.5, *)\n  @IBAction\n  func asyncIBActionWithFullBody(_: AnyObject) {\nTask { @MainActor in\n      print("Hello World")\n  \}\n\}}}

  @available(SwiftStdlib 5.5, *) @IBAction func asyncIBActionNoBody(_: AnyObject) async
  // expected-error@-1 {{expected '{' in body of function declaration}}
  // expected-error@-2 {{@IBAction instance method cannot be asynchronous}}
  // expected-note@-3 {{remove 'async' and wrap in 'Task' to use concurrency in 'asyncIBActionNoBody}}{{3-88=@available(SwiftStdlib 5.5, *) @IBAction func asyncIBActionNoBody(_: AnyObject)}}

}

struct S { }
enum E { }

protocol P1 { }
protocol P2 { }

protocol CP1 : class { }
protocol CP2 : class { }

@objc protocol OP1 { }
@objc protocol OP2 { }

// Teach the compiler that String is @objc-friendly without importing
// Foundation.
extension String: @retroactive _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC") public func _bridgeToObjectiveC() -> AnyObject { fatalError() }
  public static func _forceBridgeFromObjectiveC(_ x: AnyObject, result: inout String?) { fatalError() }
  public static func _conditionallyBridgeFromObjectiveC(_ x: AnyObject, result: inout String?) -> Bool { fatalError() }
  @_effects(readonly) public static func _unconditionallyBridgeFromObjectiveC(_ source: AnyObject? ) -> String { fatalError() }
}

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

  // Any
  @IBAction func action4a(_: Any) {}
  @IBAction func action5a(_: Any?) {}
  @IBAction func action6a(_: Any!) {}

  // Protocol types
  @IBAction func action7(_: P1) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'P1' cannot be represented in Objective-C}}
  @IBAction func action8(_: CP1) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'CP1' cannot be represented in Objective-C}}
  @IBAction func action9(_: OP1) {}
  @IBAction func action10(_: P1?) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action11(_: CP1?) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action12(_: OP1?) {}
  @IBAction func action13(_: P1!) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action14(_: CP1!) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action15(_: OP1!) {}

  // Class metatype
  @IBAction func action15b(_: X.Type) {}
  @IBAction func action16(_: X.Type?) {}
  @IBAction func action17(_: X.Type!) {}

  // AnyClass
  @IBAction func action18(_: AnyClass) {}
  @IBAction func action19(_: AnyClass?) {}
  @IBAction func action20(_: AnyClass!) {}

  // Protocol types
  @IBAction func action21(_: P1.Type) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action22(_: CP1.Type) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action23(_: OP1.Type) {}
  @IBAction func action24(_: P1.Type?) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action25(_: CP1.Type?) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action26(_: OP1.Type?) {}
  @IBAction func action27(_: P1.Type!) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action28(_: CP1.Type!) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}
  @IBAction func action29(_: OP1.Type!) {}

  // Structs representable in Objective-C
  @IBAction func action32(_: Int) {}
  @IBAction func action33(_: Int?) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}}

  @IBAction func action34(_: String) {}
  @IBAction func action35(_: String?) {}

  // Other bad cases
  @IBAction func action30(_: S) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  @IBAction func action31(_: E) {} // expected-error{{method cannot be marked '@IBAction' because the type of the parameter cannot be represented in Objective-C}} expected-note{{non-'@objc' enums cannot be represented in Objective-C}}

  init() { }
}

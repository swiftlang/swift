// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

@IBSegueAction // expected-error {{@IBSegueAction may only be used on 'func' declarations}} {{1-16=}}
var iboutlet_global: Int

var iboutlet_accessor: Int {
  @IBSegueAction // expected-error {{@IBSegueAction may only be used on 'func' declarations}} {{3-18=}}
  get { return 42 }
}

@IBSegueAction // expected-error {{@IBSegueAction may only be used on 'func' declarations}} {{1-16=}}
class IBOutletClassTy {}
@IBSegueAction // expected-error {{@IBSegueAction may only be used on 'func' declarations}} {{1-16=}}
struct IBStructTy {}

@IBSegueAction // expected-error {{only instance methods can be declared @IBSegueAction}} {{1-16=}}
func IBFunction(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {}

class IBActionWrapperTy {
  @IBSegueAction
  func click(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // no-warning

  func outer(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {
    @IBSegueAction  // expected-error {{only instance methods can be declared @IBSegueAction}} {{5-20=}}
    func inner(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()}
    fatalError()
  }
  @IBSegueAction // expected-error {{@IBSegueAction may only be used on 'func' declarations}} {{3-18=}}
  var value : AnyObject? = nil

  @IBSegueAction
  func process(x: AnyObject, _: AnyObject, _: AnyObject) {}  // expected-error {{methods declared @IBSegueAction must return a value}}

  @IBSegueAction
  func process(_: AnyObject, _: AnyObject, _: AnyObject) -> Int? {fatalError()}  // expected-error {{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}

  // @IBSegueAction does /not/ semantically imply @objc.
  @IBSegueAction // expected-note {{attribute already specified here}}
  @IBSegueAction // expected-error {{duplicate attribute}}
  func doMagic(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {}

  @IBSegueAction @objc
  func moreMagic(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {} // no-warning
  @objc @IBSegueAction
  func evenMoreMagic(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {} // no-warning

  @available(SwiftStdlib 5.5, *) @IBSegueAction
  func process(_: AnyObject, _: AnyObject, _: AnyObject) async -> AnyObject { }
  // expected-error@-1 {{@IBSegueAction instance method cannot be asynchronous}}
  // expected-note@-2 {{remove 'async' and wrap in 'Task' to use concurrency in 'process'}}{{49:3-50:80=@available(SwiftStdlib 5.5, *) @IBSegueAction\n  func process(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {\nTask { @MainActor in \}\n\}}}
}

struct S { }
enum E { case aCaseToKeepItFromBeingUninhabited }

protocol P1 { }
protocol P2 { }

protocol CP1 : class { }
protocol CP2 : class { }

@objc protocol OP1 { }
@objc protocol OP2 { }

// Check errors and fix-its for names which would cause memory leaks due to ARC
// semantics.
@objc class Leaky {
  @IBSegueAction func newScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'newScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeScreen'}} {{23-32=makeScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeScreen:'}} {{3-3=@objc(makeScreen:) }}
  @IBSegueAction func allocScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'allocScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeScreen'}} {{23-34=makeScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeScreen:'}} {{3-3=@objc(makeScreen:) }}
  @IBSegueAction func initScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'initScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeScreen'}} {{23-33=makeScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeScreen:'}} {{3-3=@objc(makeScreen:) }}
  @IBSegueAction func copyScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'copyScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeCopyScreen'}} {{23-33=makeCopyScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeCopyScreen:'}} {{3-3=@objc(makeCopyScreen:) }}
  @IBSegueAction func mutableCopyScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'mutableCopyScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeMutableCopyScreen'}} {{23-40=makeMutableCopyScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeMutableCopyScreen:'}} {{3-3=@objc(makeMutableCopyScreen:) }}
  @IBSegueAction func newScreen(_: AnyObject, secondArg: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'newScreen:secondArg:' because it has special memory management behavior}}
  // expected-note@-2{{change Swift name to 'makeScreen(_:secondArg:)'}} {{23-32=makeScreen}}
  // expected-note@-3{{change Objective-C selector to 'makeScreen:secondArg:'}} {{3-3=@objc(makeScreen:secondArg:) }}

  // If there's an @objc(explicitSelector:) already, we should validate and fix
  // that instead of the Swift name.
  @objc(actuallyOkayScreen:)
  @IBSegueAction func newActuallyOkayScreen(_: AnyObject) -> AnyObject {fatalError()}

  @objc(actuallyOkayScreen:secondArg:)
  @IBSegueAction func newActuallyOkayScreen(_: AnyObject, secondArg: AnyObject) -> AnyObject {fatalError()}

  @objc(newProblematicScreen:)
  @IBSegueAction func problematicScreen(_: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'newProblematicScreen:' because it has special memory management behavior}}
  // expected-note@-2{{change Objective-C selector to 'makeProblematicScreen:'}} {{-1:9-30=makeProblematicScreen:}}

  @objc(newProblematicScreen:secondArg:)
  @IBSegueAction func problematicScreen(_: AnyObject, secondArg: AnyObject) -> AnyObject {fatalError()}
  // expected-error@-1{{@IBSegueAction method cannot have selector 'newProblematicScreen:secondArg:' because it has special memory management behavior}}
  // expected-note@-2{{change Objective-C selector to 'makeProblematicScreen:secondArg:'}} {{-1:9-40=makeProblematicScreen:secondArg:}}
}

// Check which argument types @IBSegueAction can take.
@objc class X {
  // Class type
  @IBSegueAction func action1(_: X, _: X, _: X) -> X {fatalError()}

  @IBSegueAction func action2(_: X?, _: X?, _: X?) -> X? {fatalError()}

  @IBSegueAction func action3(_: X!, _: X!, _: X!) -> X! {fatalError()}

  // AnyObject
  @IBSegueAction func action4(_: AnyObject, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()}

  @IBSegueAction func action5(_: AnyObject?, _: AnyObject?, _: AnyObject?) -> AnyObject? {fatalError()}

  @IBSegueAction func action6(_: AnyObject!, _: AnyObject!, _: AnyObject!) -> AnyObject! {fatalError()}

  // Any
  @IBSegueAction func action4a(_: Any, _: Any, _: Any) -> Any {fatalError()}

  @IBSegueAction func action5a(_: Any?, _: Any?, _: Any?) -> Any? {fatalError()}

  @IBSegueAction func action6a(_: Any!, _: Any!, _: Any!) -> Any! {fatalError()}

  // Protocol types
  @IBSegueAction func action7a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1 {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'P1' cannot be represented in Objective-C}}
  @IBSegueAction func action7b(_: P1, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'P1' cannot be represented in Objective-C}}
  @IBSegueAction func action7c(_: AnyObject, _: P1, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'P1' cannot be represented in Objective-C}}
  @IBSegueAction func action7d(_: AnyObject, _: AnyObject, _: P1) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'P1' cannot be represented in Objective-C}}

  @IBSegueAction func action8a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1 {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'CP1' cannot be represented in Objective-C}}
  @IBSegueAction func action7b(_: CP1, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'CP1' cannot be represented in Objective-C}}
  @IBSegueAction func action7c(_: AnyObject, _: CP1, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'CP1' cannot be represented in Objective-C}}
  @IBSegueAction func action7d(_: AnyObject, _: AnyObject, _: CP1) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}} expected-note{{protocol-constrained type containing protocol 'CP1' cannot be represented in Objective-C}}

  @IBSegueAction func action9(_: OP1, _: OP1, _: OP1) -> OP1 {fatalError()}

  @IBSegueAction func action10a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1? {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action10b(_: P1?, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action10c(_: AnyObject, _: P1?, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action10d(_: AnyObject, _: AnyObject, _: P1?) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action11a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1? {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action11b(_: CP1?, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action11c(_: AnyObject, _: CP1?, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action11d(_: AnyObject, _: AnyObject, _: CP1?) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action12(_: OP1?, _: OP1?, _: OP1?) -> OP1? {fatalError()}

  @IBSegueAction func action13a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1! {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action13b(_: P1!, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action13c(_: AnyObject, _: P1!, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action13d(_: AnyObject, _: AnyObject, _: P1!) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action14a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1! {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action14b(_: CP1!, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action14c(_: AnyObject, _: CP1!, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action14d(_: AnyObject, _: AnyObject, _: CP1!) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action15(_: OP1!, _: OP1!, _: OP1!) -> OP1! {fatalError()}

  // Class metatype
  @IBSegueAction func action16a(_: X.Type, _: X.Type, _: X.Type) -> X.Type {fatalError()}
  @IBSegueAction func action16b(_: X.Type?, _: X.Type?, _: X.Type?) -> X.Type? {fatalError()}
  @IBSegueAction func action16c(_: X.Type!, _: X.Type!, _: X.Type!) -> X.Type! {fatalError()}

  // AnyClass
  @IBSegueAction func action18(_: AnyClass, _: AnyClass, _: AnyClass) -> AnyClass {fatalError()}
  @IBSegueAction func action19(_: AnyClass?, _: AnyClass?, _: AnyClass?) -> AnyClass? {fatalError()}
  @IBSegueAction func action20(_: AnyClass!, _: AnyClass!, _: AnyClass!) -> AnyClass! {fatalError()}

  // Protocol types
  @IBSegueAction func action21a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1.Type {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action21b(_: P1.Type, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action21c(_: AnyObject, _: P1.Type, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action21d(_: AnyObject, _: AnyObject, _: P1.Type) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action22a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1.Type {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action22b(_: CP1.Type, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action22c(_: AnyObject, _: CP1.Type, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action22d(_: AnyObject, _: AnyObject, _: CP1.Type) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action23(_: OP1.Type, _: OP1.Type, _: OP1.Type) -> OP1.Type {fatalError()}

  @IBSegueAction func action24a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1.Type? {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action24b(_: P1.Type?, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action24c(_: AnyObject, _: P1.Type?, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action24d(_: AnyObject, _: AnyObject, _: P1.Type?) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action25a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1.Type? {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action25b(_: CP1.Type?, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action25c(_: AnyObject, _: CP1.Type?, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action25d(_: AnyObject, _: AnyObject, _: CP1.Type?) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action26(_: OP1.Type?, _: OP1.Type?, _: OP1.Type?) -> OP1.Type? {fatalError()}

  @IBSegueAction func action27a(_: AnyObject, _: AnyObject, _: AnyObject) -> P1.Type! {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action27b(_: P1.Type!, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action27c(_: AnyObject, _: P1.Type!, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action27d(_: AnyObject, _: AnyObject, _: P1.Type!) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action28a(_: AnyObject, _: AnyObject, _: AnyObject) -> CP1.Type! {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}}
  @IBSegueAction func action28b(_: CP1.Type!, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}}
  @IBSegueAction func action28c(_: AnyObject, _: CP1.Type!, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}}
  @IBSegueAction func action28d(_: AnyObject, _: AnyObject, _: CP1.Type!) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}}

  @IBSegueAction func action29(_: OP1.Type!, _: OP1.Type!, _: OP1.Type!) -> OP1.Type! {fatalError()}

  // Other bad cases
  @IBSegueAction func action30a(_: AnyObject, _: AnyObject, _: AnyObject) -> S {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  @IBSegueAction func action30b(_: S, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  @IBSegueAction func action30c(_: AnyObject, _: S, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  @IBSegueAction func action30d(_: AnyObject, _: AnyObject, _: S) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  @IBSegueAction func action31a(_: AnyObject, _: AnyObject, _: AnyObject) -> E {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because its result type cannot be represented in Objective-C}} expected-note{{non-'@objc' enums cannot be represented in Objective-C}}
  @IBSegueAction func action31b(_: E, _: AnyObject, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 1 cannot be represented in Objective-C}} expected-note{{non-'@objc' enums cannot be represented in Objective-C}}
  @IBSegueAction func action31c(_: AnyObject, _: E, _: AnyObject) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 2 cannot be represented in Objective-C}} expected-note{{non-'@objc' enums cannot be represented in Objective-C}}
  @IBSegueAction func action31d(_: AnyObject, _: AnyObject, _: E) -> AnyObject {fatalError()} // expected-error{{method cannot be marked '@IBSegueAction' because the type of the parameter 3 cannot be represented in Objective-C}} expected-note{{non-'@objc' enums cannot be represented in Objective-C}}

  // Supported arities
  @IBSegueAction func actionWith0() -> X {fatalError()} // expected-error{{@IBSegueAction methods must have 1 to 3 arguments}}
  @IBSegueAction func actionWith1(_: X) -> X {fatalError()}
  @IBSegueAction func actionWith2(_: X, _: X) -> X {fatalError()}
  @IBSegueAction func actionWith3(_: X, _: X, _: X) -> X {fatalError()}
  @IBSegueAction func actionWith4(_: X, _: X, _: X, _: X) -> X {fatalError()} // expected-error{{@IBSegueAction methods must have 1 to 3 arguments}}

  init() { }
}

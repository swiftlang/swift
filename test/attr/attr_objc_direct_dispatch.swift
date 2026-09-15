// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect

import Foundation

// A direct method is absent from its class's Objective-C method list, so any
// construct that reaches a method by selector at runtime is unsound against it.

class DispatchConflicts: NSObject {
  // 'final' does not exclude 'dynamic': IsDynamicRequest honours an explicit
  // 'dynamic' before it consults isSemanticallyFinal(), so this has to be
  // rejected on its own. Checked before the 'final' rule, hence no 'final'
  // here -- if the order were wrong this would report "must be final" instead.
  @objcDirect dynamic func dynamicMethod() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' method; 'dynamic' requires dispatch through the Objective-C runtime}}

  // An action is wired up by selector from a nib or storyboard, which the
  // compiler never sees.
  @objcDirect @IBAction func action(_ sender: Any) {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBAction' or '@IBSegueAction' method; the action is invoked by selector}}

  @objcDirect @IBSegueAction func segueAction(_ coder: NSCoder) -> Any? { return nil }
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBAction' or '@IBSegueAction' method; the action is invoked by selector}}

  // Control: none of the above applies, so this is accepted.
  @objcDirect final func plainDirect() {}
}

// An @objc protocol requirement is dispatched by selector, so a direct witness
// would fail at every call made through the protocol -- far from the
// declaration that carries the attribute.
@objc protocol Greeter {
  func greet() // expected-note {{requirement 'greet()' declared here}}
  func wave()
}

class DirectGreeter: NSObject, Greeter {
  @objcDirect final func greet() {}
  // expected-error@-1 {{cannot satisfy a requirement of '@objc' protocol 'Greeter' because it is a direct method}}

  // Control: a normal @objc witness is unaffected.
  func wave() {}
}

// AnyObject dispatch sends by selector, so a direct method must not be a
// candidate. It is excluded from the lookup rather than diagnosed, matching
// what the compiler already does for methods imported from Clang's
// objc_direct -- which turns the call into a compile error at the use site
// instead of an unrecognized selector at runtime.
//
// Known limitation: if some *other* visible type has a non-direct method with
// the same name and signature, that one is found instead and the call compiles.
// Sending it to an instance of the direct class then fails at runtime, which is
// the ambient hazard of AnyObject dispatch rather than one this attribute adds.
@objc class DirectOnly: NSObject {
  @objcDirect final func onlyDirect() -> Int { return 0 }
  @objc final func alsoNormal() -> Int { return 0 }
}

func anyObjectDispatch(_ x: AnyObject) {
  let _ = x.onlyDirect()
  // expected-error@-1 {{value of type 'AnyObject' has no member 'onlyDirect'}}

  // Control: the sibling @objc method is still reachable dynamically.
  let _ = x.alsoNormal()
}

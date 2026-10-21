// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect

import Foundation

// MARK: - Runtime dispatch

class DispatchConflicts: NSObject {
  @objcDirect dynamic func dynamicMethod() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' instance method; 'dynamic' requires dispatch through the Objective-C runtime}}

  @objcDirect @IBAction func action(_ sender: Any) {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBAction' or '@IBSegueAction' method; the action is invoked by selector}}

  @objcDirect @IBSegueAction func segueAction(_ coder: NSCoder) -> Any? { return nil }
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBAction' or '@IBSegueAction' method; the action is invoked by selector}}

  @objcDirect @IBOutlet var outlet: NSObject?
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBOutlet', '@IBInspectable' or '@GKInspectable' property; the value is set by key through key-value coding}}

  @objcDirect @IBInspectable var inspectable: Int = 0
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBOutlet', '@IBInspectable' or '@GKInspectable' property; the value is set by key through key-value coding}}

  @objcDirect @GKInspectable var gkInspectable: Int = 0
  // expected-error@-1 {{'@objcDirect' cannot be applied to an '@IBOutlet', '@IBInspectable' or '@GKInspectable' property; the value is set by key through key-value coding}}

  @objcDirect final func plainDirect() {}
  @objcDirect var plainDirectProperty: NSObject?
}

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

// AnyObject lookup skips direct methods, as it does Clang's objc_direct ones.
// A same-named method of another type is still found, and fails at run time.
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

// MARK: - #selector

// Two '#selector' uses give two notes on the declaration's line.
@objc class SelectorTarget: NSObject {
  // expected-note@+1 2 {{a direct method is not added to its class's Objective-C method list}}
  @objcDirect final func directHandler() {}
  // expected-note@+1 {{a direct method is not added to its class's Objective-C method list}}
  @objcDirect public final func publicDirectHandler() {}

  @objc final func normalHandler() {}
}

func formSelectors(_ target: SelectorTarget) {
  _ = #selector(SelectorTarget.directHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'directHandler()' that is a direct method}}

  _ = #selector(SelectorTarget.publicDirectHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'publicDirectHandler()' that is a direct method}}

  _ = #selector(target.directHandler)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'directHandler()' that is a direct method}}

  _ = #selector(SelectorTarget.normalHandler)
}

// One note per accessor, both on the property.
@objc class PropertySelectorTarget: NSObject {
  // expected-note@+1 2 {{a direct property's accessor is not added to its class's Objective-C method list}}
  @objcDirect var directValue: Int = 0
}

func formAccessorSelectors() {
  _ = #selector(getter: PropertySelectorTarget.directValue)
  // expected-error@-1 {{argument of '#selector' refers to property 'directValue' whose accessors are direct}}
  _ = #selector(setter: PropertySelectorTarget.directValue)
  // expected-error@-1 {{argument of '#selector' refers to property 'directValue' whose accessors are direct}}
}

// MARK: - #keyPath

// Two key paths give two notes on the declaration's line.

@objc class KeyPathTarget: NSObject {
  // expected-note@+1 2 {{key-value coding cannot reach the property}}
  @objcDirect var directProperty: Int = 0

  @objc var normalProperty: Int = 0
}

func formKeyPaths() {
  _ = #keyPath(KeyPathTarget.directProperty)
  // expected-error@-1 {{argument of '#keyPath' refers to property 'directProperty' that is direct}}

  _ = #keyPath(KeyPathTarget.normalProperty)
}

// The rejection follows the property through a multi-component path.
@objc class Outer: NSObject {
  @objc var inner: KeyPathTarget = KeyPathTarget()
}

func nestedKeyPath() {
  _ = #keyPath(Outer.inner.directProperty)
  // expected-error@-1 {{argument of '#keyPath' refers to property 'directProperty' that is direct}}

  _ = #keyPath(Outer.inner.normalProperty)
}

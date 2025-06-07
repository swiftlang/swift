// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: objc_interop

import Foundation

// @NSCopying -- banned in @abi
class NSCopyingAttr: NSObject {
  @abi(@NSCopying var v1: NSArray?) // expected-error {{unused 'NSCopying' attribute in '@abi'}} {{8-18=}}
  @NSCopying var v1: NSArray? = nil

  @abi(var v2: NSArray?)
  @NSCopying var v2: NSArray? = nil
}

// Availability
// These tests will only work on a versioned platform.
@available(macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99, *)
struct FutureType {}

@abi(func available5(_: FutureType)) // expected-error {{'FutureType' is only available in }}
func available5(_: FutureType) {} // expected-error {{'FutureType' is only available in }}
// expected-note@-1 2{{add '@available' attribute to enclosing global function}} (fix-it not tested because it varies by target)

@abi(func available6(_: FutureType))
@available(macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99, *) func available6(_: FutureType) {}

// @objc -- banned in @abi
class ObjCAttr: NSObject {
  @abi(@objc func fn1()) // expected-error {{unused 'objc' attribute in '@abi'}} {{8-13=}}
  @objc func fn1() {}

  @abi(@objc func fn2()) // expected-error {{unused 'objc' attribute in '@abi'}} {{8-13=}}
  func fn2() {}

  @abi(func fn3())
  @objc func fn3() {}
}

// @IBAction -- banned in @abi
class IBActionAttr: NSObject {
  @abi(@IBAction func fn1(_: Any)) // expected-error {{unused 'IBAction' attribute in '@abi'}} {{8-17=}}
  @IBAction func fn1(_: Any) {}

  @abi(@IBAction func fn2(_: Any)) // expected-error {{unused 'IBAction' attribute in '@abi'}} {{8-17=}}
  func fn2(_: Any) {}

  @abi(func fn3(_: Any))
  @IBAction func fn3(_: Any) {}
}

// @IBInspectable -- banned in @abi
class IBInspectableAttr: NSObject {
  @abi(@IBInspectable var v1: Double) // expected-error {{unused 'IBInspectable' attribute in '@abi'}} {{8-22=}}
  @IBInspectable var v1: Double = 0.0

  @abi(@IBInspectable var v2: Double) // expected-error {{unused 'IBInspectable' attribute in '@abi'}} {{8-22=}}
  var v2: Double = 0.0

  @abi(var v3: Double)
  @IBInspectable var v3: Double = 0.0
}

// @GKInspectable -- banned in @abi
class GKInspectableAttr: NSObject {
  @abi(@GKInspectable var v1: Double) // expected-error {{unused 'GKInspectable' attribute in '@abi'}} {{8-22=}}
  @GKInspectable var v1: Double = 0.0

  @abi(@GKInspectable var v2: Double) // expected-error {{unused 'GKInspectable' attribute in '@abi'}} {{8-22=}}
  var v2: Double = 0.0

  @abi(var v3: Double)
  @GKInspectable var v3: Double = 0.0
}

// @IBOutlet -- banned in @abi
class IBOutletAttr: NSObject {
  @abi(@IBOutlet var v1: NSObject!) // expected-error {{unused 'IBOutlet' attribute in '@abi'}} {{8-17=}}
  @IBOutlet var v1: NSObject!

  @abi(@IBOutlet var v2: NSObject!) // expected-error {{unused 'IBOutlet' attribute in '@abi'}} {{8-17=}}
  var v2: NSObject!

  @abi(var v3: NSObject!)
  @IBOutlet var v3: NSObject!
}

// @IBSegueAction -- banned in @abi
class IBSegueActionAttr: NSObject {
  @abi(@IBSegueAction func fn1(_: Any) -> Any) // expected-error {{unused 'IBSegueAction' attribute in '@abi'}} {{8-22=}}
  @IBSegueAction func fn1(_: Any) -> Any {}

  @abi(@IBSegueAction func fn2(_: Any) -> Any) // expected-error {{unused 'IBSegueAction' attribute in '@abi'}} {{8-22=}}
  func fn2(_: Any) -> Any {}

  @abi(func fn3(_: Any) -> Any)
  @IBSegueAction func fn3(_: Any) -> Any {}
}

// @NSManaged -- banned in @abi
class NSManagedAttr: NSObject {
  @abi(@NSManaged var v1: NSObject!) // expected-error {{unused 'NSManaged' attribute in '@abi'}} {{8-18=}}
  @NSManaged var v1: NSObject!

  @abi(@NSManaged var v2: NSObject!) // expected-error {{unused 'NSManaged' attribute in '@abi'}} {{8-18=}}
  var v2: NSObject!

  @abi(var v3: NSObject!)
  @NSManaged var v3: NSObject!
}

// @nonobjc -- banned in @abi
@objcMembers
class NonObjCAttr: NSObject {
  @abi(@nonobjc var v1: NSObject!) // expected-error {{unused 'nonobjc' attribute in '@abi'}} {{8-16=}}
  @nonobjc var v1: NSObject!

  @abi(@nonobjc var v2: NSObject!) // expected-error {{unused 'nonobjc' attribute in '@abi'}} {{8-16=}}
  var v2: NSObject!

  @abi(var v3: NSObject!)
  @nonobjc var v3: NSObject!
}

// optional -- banned in @abi
@objc protocol OptionalModifier {
  @abi(
    @objc // expected-error {{unused 'objc' attribute in '@abi'}} {{5-11=}}
    optional // expected-error {{unused 'optional' modifier in '@abi'}} {{5-14=}}
    func fn1()
  )
  @objc optional func fn1()

  @abi(
    @objc // expected-error {{unused 'objc' attribute in '@abi'}} {{5-11=}}
    optional // expected-error {{unused 'optional' modifier in '@abi'}} {{5-14=}}
    func fn2()
  )
  @objc func fn2()

  @abi(func fn3())
  @objc optional func fn3()
}

// dynamic -- banned in @abi
class DynamicModifier: NSObject {
  @abi(dynamic func fn1()) // expected-error {{unused 'dynamic' modifier in '@abi'}} {{8-15=}}
  dynamic func fn1() {}

  @abi(dynamic func fn2()) // expected-error {{unused 'dynamic' modifier in '@abi'}} {{8-15=}}
  func fn2() {}

  @abi(func fn3())
  dynamic func fn3() {}
}

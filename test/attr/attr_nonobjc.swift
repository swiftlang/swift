// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation

@objc class LightSaber {
  init() {
    caloriesBurned = 5
  }

  @objc func defeatEnemy(_ b: Bool) -> Bool { // expected-note {{'defeatEnemy' previously declared here}}
    return !b
  }

  // Make sure we can overload a method with @nonobjc methods
  @nonobjc func defeatEnemy(_ i: Int) -> Bool {
    return (i > 0)
  }

  // This is not allowed, though
  @objc func defeatEnemy(_ s: String) -> Bool { // expected-error {{method 'defeatEnemy' with Objective-C selector 'defeatEnemy:' conflicts with previous declaration with the same Objective-C selector}}
    return s != ""
  }

  @nonobjc subscript(index: Int) -> Int {
    return index
  }

  @nonobjc var caloriesBurned: Float
}

class BlueLightSaber : LightSaber {
  @nonobjc override func defeatEnemy(_ b: Bool) -> Bool { }
}

@objc class InchoateToad {
  @objc init(x: Int) {} // expected-note {{previously declared}}
  @nonobjc init(x: Float) {}
  @objc init(x: String) {} // expected-error {{conflicts with previous declaration with the same Objective-C selector}}
}

@nonobjc class NonObjCClassNotAllowed { } // expected-error {{'@nonobjc' attribute cannot be applied to this declaration}} {{1-10=}}

class NonObjCDeallocNotAllowed {
  @nonobjc deinit { // expected-error {{'@nonobjc' attribute cannot be applied to this declaration}} {{3-12=}}

  }
}

@objc protocol ObjCProtocol {
  func protocolMethod() // expected-note {{}}

  @nonobjc func nonObjCProtocolMethodNotAllowed() // expected-error {{declaration is a member of an '@objc' protocol, and cannot be marked '@nonobjc'}}

  @nonobjc subscript(index: Int) -> Int { get } // expected-error {{declaration is a member of an '@objc' protocol, and cannot be marked '@nonobjc'}}

  var surfaceArea: Float { @nonobjc get } // expected-error {{declaration is implicitly '@objc', and cannot be marked '@nonobjc'}}

  var displacement: Float { get }
}

class SillyClass {
  @objc var description: String { @nonobjc get { return "" } } // expected-error {{declaration is implicitly '@objc', and cannot be marked '@nonobjc'}}
}

class ObjCAndNonObjCNotAllowed {
  @objc @nonobjc func redundantAttributes() { } // expected-error {{declaration is marked '@objc', and cannot be marked '@nonobjc'}}
}

class DynamicAndNonObjCAreFineNow {
  @nonobjc dynamic func someAttributes() { }
}

class IBOutletAndNonObjCNotAllowed {
  @nonobjc @IBOutlet var leeloo : String? = "Hello world" // expected-error {{declaration is marked '@IBOutlet', and cannot be marked '@nonobjc'}}
}

class NSManagedAndNonObjCNotAllowed {
  @nonobjc @NSManaged var rosie : NSObject // expected-error {{declaration is marked '@NSManaged', and cannot be marked '@nonobjc'}}
}

@nonobjc func nonObjCTopLevelFuncNotAllowed() { } // expected-error {{only class members and extensions of classes can be declared '@nonobjc'}} {{1-10=}}

@objc class NonObjCPropertyObjCProtocolNotAllowed : ObjCProtocol { // expected-error {{does not conform to protocol}} expected-note {{add stubs for conformance}}
  @nonobjc func protocolMethod() { } // expected-note {{candidate is explicitly '@nonobjc'}}

  func nonObjCProtocolMethodNotAllowed() { }

  subscript(index: Int) -> Int {
    return index
  }

  var displacement: Float {
    @nonobjc get { // expected-error {{declaration is implicitly '@objc', and cannot be marked '@nonobjc'}}
      return Float(self[10])
    }
  }

  var surfaceArea: Float {
    get {
      return Float(100)
    }
  }
}

struct SomeStruct { }
@nonobjc extension SomeStruct { } // expected-error{{only extensions of classes can be declared '@nonobjc'}}

// https://github.com/apple/swift/issues/46809

protocol P_46809 : class {}
extension P_46809 {
  @nonobjc func function() {} // expected-error {{only class members and extensions of classes can be declared '@nonobjc'}}
}

@objc enum SomeEnum: Int {
  @nonobjc case what // expected-error {{'@nonobjc' attribute cannot be applied to this declaration}}
}

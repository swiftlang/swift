// RUN: %target-parse-verify-swift
// REQUIRES: objc_interop

import Foundation

@objc class LightSaber {
  init() {
    caloriesBurned = 5
  }

  func defeatEnemy(b: Bool) -> Bool { // expected-note {{'defeatEnemy' previously declared here}}
    return !b
  }

  // Make sure we can overload a method with @nonobjc methods
  @nonobjc func defeatEnemy(i: Int) -> Bool {
    return (i > 0)
  }

  // This is not allowed, though
  func defeatEnemy(s: String) -> Bool { // expected-error {{method 'defeatEnemy' with Objective-C selector 'defeatEnemy:' conflicts with previous declaration with the same Objective-C selector}}
    return (count(s) > 0)
  }

  @nonobjc subscript(index: Int) -> Int {
    return index
  }

  @nonobjc var caloriesBurned: Float
}

class BlueLightSaber : LightSaber {
  @nonobjc override func defeatEnemy(b: Bool) -> Bool { } // expected-error {{declaration is implicitly @objc, and cannot be marked @nonobjc}}
}

@objc class InchoateToad {
  init(x: Int) {} // expected-note {{previously declared}}
  @nonobjc init(x: Float) {}
  init(x: String) {} // expected-error {{conflicts with previous declaration with the same Objective-C selector}}
}

@nonobjc class NonObjCClassNotAllowed { } // expected-error {{@nonobjc cannot be applied to this declaration}}

class NonObjCDeallocNotAllowed {
  @nonobjc deinit { // expected-error {{@nonobjc cannot be applied to this declaration}}

  }
}

@objc protocol ObjCProtocol {
  func protocolMethod() // expected-note {{}}

  @nonobjc func nonObjCProtocolMethodNotAllowed() // expected-error {{declaration is a member of an @objc protocol, and cannot be marked @nonobjc}}

  @nonobjc subscript(index: Int) -> Int { get } // expected-error {{declaration is a member of an @objc protocol, and cannot be marked @nonobjc}}

  var displacement: Float { get }
}

class ObjCAndNonObjCNotAllowed {
  @objc @nonobjc func redundantAttributes() { } // expected-error {{declaration is marked @objc, and cannot be marked @nonobjc}}
}

class DynamicAndNonObjCNotAllowed {
  @nonobjc dynamic func redundantAttributes() { } // expected-error {{declaration is marked dynamic, and cannot be marked @nonobjc}}
}

class IBOutletAndNonObjCNotAllowed {
  @nonobjc @IBOutlet var leeloo : String? = "Hello world" // expected-error {{declaration is marked @IBOutlet, and cannot be marked @nonobjc}}
}

class NSManagedAndNonObjCNotAllowed {
  @nonobjc @NSManaged var rosie : NSObject // expected-error {{declaration is marked @NSManaged, and cannot be marked @nonobjc}}
}

@nonobjc func nonObjCTopLevelFuncNotAllowed() { } // expected-error {{only methods can be declared @nonobjc}}

@objc class NonObjCPropertyObjCProtocolNotAllowed : ObjCProtocol { // expected-error {{does not conform to protocol}}
  @nonobjc func protocolMethod() { } // expected-note {{candidate is not '@objc', but protocol requires it}}

  func nonObjCProtocolMethodNotAllowed() { }

  subscript(index: Int) -> Int {
    return index
  }

  var displacement: Float {
    @nonobjc get { // expected-error {{declaration is implicitly @objc, and cannot be marked @nonobjc}}
      return Float(self[10])
    }
  }
}

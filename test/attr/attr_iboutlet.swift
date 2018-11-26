// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop
import Foundation

// expected-error@+1 {{@IBOutlet property cannot have non-object type 'Int'}}
@IBOutlet // expected-error {{only instance properties can be declared @IBOutlet}} {{1-11=}}
var iboutlet_global: Int?

@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}} {{1-11=}}
class IBOutletClassTy {}
@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}} {{1-11=}}
struct IBStructTy {}

@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}} {{1-11=}}
func IBFunction() -> () {}

@objc
class IBOutletWrapperTy {
  @IBOutlet
  var value : IBOutletWrapperTy! = IBOutletWrapperTy() // no-warning

  @IBOutlet
  class var staticValue: IBOutletWrapperTy? = 52  // expected-error {{cannot convert value of type 'Int' to specified type 'IBOutletWrapperTy?'}}
  // expected-error@-2 {{only instance properties can be declared @IBOutlet}} {{3-12=}}
  // expected-error@-2 {{class stored properties not supported}}

  @IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}} {{3-13=}}
  func click() -> () {}

  @IBOutlet // expected-error {{@IBOutlet attribute requires property to be mutable}} {{3-13=}}
  let immutable: IBOutletWrapperTy? = nil

  @IBOutlet // expected-error {{@IBOutlet attribute requires property to be mutable}} {{3-13=}}
  var computedImmutable: IBOutletWrapperTy? {
    return nil
  }
}

struct S { }
enum E { }

protocol P1 { }
protocol P2 { }

protocol CP1 : class { }
protocol CP2 : class { }

@objc protocol OP1 { }
@objc protocol OP2 { }

class NonObjC {}

// Ensure that only ObjC types work
@objc class X {
  // Class type
  @IBOutlet var outlet2: X?
  @IBOutlet var outlet3: X!

  @IBOutlet var outlet2a: NonObjC? // expected-error{{@IBOutlet property cannot have non-'@objc' class type 'NonObjC'}} {{3-13=}}
  @IBOutlet var outlet3a: NonObjC! // expected-error{{@IBOutlet property cannot have non-'@objc' class type 'NonObjC'}} {{3-13=}}

  // AnyObject
  @IBOutlet var outlet5: AnyObject?
  @IBOutlet var outlet6: AnyObject!

  // Any
  @IBOutlet var outlet5a: Any?
  @IBOutlet var outlet6a: Any!

  // Protocol types
  @IBOutlet var outlet10: P1? // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}} {{3-13=}}
  @IBOutlet var outlet11: CP1? // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}} {{3-13=}}
  @IBOutlet var outlet12: OP1?
  @IBOutlet var outlet13: P1! // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}} {{3-13=}}
  @IBOutlet var outlet14: CP1! // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}} {{3-13=}}
  @IBOutlet var outlet15: OP1!

  // Class metatype
  @IBOutlet var outlet16: X.Type? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet17: X.Type! // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}

  // AnyClass
  @IBOutlet var outlet19: AnyClass? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet20: AnyClass! // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}

  // Protocol metatype types
  @IBOutlet var outlet24: P1.Type? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet25: CP1.Type? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet26: OP1.Type? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet27: P1.Type! // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet28: CP1.Type! // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet29: OP1.Type! // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  
  // String
  @IBOutlet var outlet33: String?
  @IBOutlet var outlet34: String!

  // Other bad cases
  @IBOutlet var outlet35: S? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet36: E? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet37: (X, X)? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}
  @IBOutlet var outlet38: Int? // expected-error{{@IBOutlet property cannot have non-object type}} {{3-13=}}

  @IBOutlet var collection1b: [AnyObject]?
  @IBOutlet var collection1c: [AnyObject]!

  @IBOutlet var collection2b: [X]?
  @IBOutlet var collection2c: [X]!

  @IBOutlet var collection3b: [OP1]?
  @IBOutlet var collection3c: [OP1]!

  @IBOutlet var collection4b: ([CP1])? // expected-error{{@IBOutlet property cannot be an array of non-'@objc' protocol type}} {{3-13=}}
  @IBOutlet var collection4c: ([CP1])! // expected-error{{@IBOutlet property cannot be an array of non-'@objc' protocol type}} {{3-13=}}

  @IBOutlet var collection5b: ([String])?
  @IBOutlet var collection5c: ([String])!

  @IBOutlet var collection6b: ([NonObjC])? // expected-error{{@IBOutlet property cannot be an array of non-'@objc' class type}} {{3-13=}}
  @IBOutlet var collection6c: ([NonObjC])! // expected-error{{@IBOutlet property cannot be an array of non-'@objc' class type}} {{3-13=}}

  init() { }
}

// Check that the type is optional
@objc class OX {
  // expected-error@+3 {{@IBOutlet property has non-optional type 'OX'}}
  // expected-note @+2 {{add '?' to form the optional type 'OX?'}}
  // expected-note @+1 {{add '!' to form an implicitly unwrapped optional}}
  @IBOutlet var ox: OX
  init() { }
}

// Check reference storage types
@objc class RSX {
  @IBOutlet weak var rsx1: RSX?
  @IBOutlet unowned var rsx2: RSX?
  @IBOutlet unowned(unsafe) var rsx3: RSX?
  init() { }
}

@objc class Infer {
  @IBOutlet var outlet1: Infer!
  @IBOutlet weak var outlet2: Infer!

  func testOptionalNess() {
   _ = outlet1!
   _ = outlet2!
  }

  func testUnchecked() {
   _ = outlet1
   _ = outlet2
  }
  
  // This outlet is strong and optional.
  @IBOutlet var outlet4: AnyObject?

  func testStrong() {
    if outlet4 != nil {}
  }
}

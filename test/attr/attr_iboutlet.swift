// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

@IBOutlet // expected-error {{only instance properties can be declared @IBOutlet}}
var iboutlet_global: Int

@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}}
class IBOutletClassTy {}
@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}}
struct IBStructTy {}

@IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}}
func IBFunction() -> () {}

@objc
class IBOutletWrapperTy {
  @IBOutlet
  var value : IBOutletWrapperTy! = IBOutletWrapperTy() // no-warning

  @IBOutlet
  class var staticValue: IBOutletWrapperTy = 52  // expected-error {{'Int' is not convertible to 'IBOutletWrapperTy'}}
  // expected-error@-2 {{only instance properties can be declared @IBOutlet}}
  // expected-error@-2 {{class stored properties not yet supported}}

  @IBOutlet // expected-error {{@IBOutlet may only be used on 'var' declarations}}
  func click() -> () {}

  @IBOutlet // expected-error {{@IBOutlet attribute requires property to be mutable}}
  let immutable: IBOutletWrapperTy? = nil

  @IBOutlet // expected-error {{@IBOutlet attribute requires property to be mutable}}
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

// Check where @IBOutlet can occur
@objc class X {
  // Class type
  @IBOutlet var outlet2: X?
  @IBOutlet var outlet3: X!

  @IBOutlet var outlet1a: NonObjC // expected-error{{@IBOutlet property cannot have non-'@objc' class type 'NonObjC'}}
  @IBOutlet var outlet2a: NonObjC? // expected-error{{@IBOutlet property cannot have non-'@objc' class type 'NonObjC'}}
  @IBOutlet var outlet3a: NonObjC! // expected-error{{@IBOutlet property cannot have non-'@objc' class type 'NonObjC'}}

  // AnyObject
  @IBOutlet var outlet5: AnyObject?
  @IBOutlet var outlet6: AnyObject!

  // Protocol types
  @IBOutlet var outlet7: P1 // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type 'P1'}}
  @IBOutlet var outlet8: CP1 // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type 'CP1'}}
  @IBOutlet var outlet10: P1? // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}}
  @IBOutlet var outlet11: CP1? // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}}
  @IBOutlet var outlet12: OP1?
  @IBOutlet var outlet13: P1! // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}}
  @IBOutlet var outlet14: CP1! // expected-error{{@IBOutlet property cannot have non-'@objc' protocol type}}
  @IBOutlet var outlet15: OP1!

  // Class metatype
  @IBOutlet var outlet15b: X.Type // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet16: X.Type? // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet17: X.Type! // expected-error{{@IBOutlet property cannot have non-object type}}

  // AnyClass
  @IBOutlet var outlet18: AnyClass // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet19: AnyClass? // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet20: AnyClass! // expected-error{{@IBOutlet property cannot have non-object type}}

  // Protocol types
  @IBOutlet var outlet21: P1.Type // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet22: CP1.Type // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet23: OP1.Type // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet24: P1.Type? // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet25: CP1.Type? // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet26: OP1.Type? // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet27: P1.Type! // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet28: CP1.Type! // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet29: OP1.Type! // expected-error{{@IBOutlet property cannot have non-object type}}
  
  // weak/unowned
  @IBOutlet weak var outlet30: X?
  @IBOutlet weak var outlet31: X!

  // String
  @IBOutlet var outlet33: String?
  @IBOutlet var outlet34: String!

  // Other bad cases
  @IBOutlet var outlet35: S // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet36: E // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet37: (X, X)  // expected-error{{@IBOutlet property cannot have non-object type}}
  @IBOutlet var outlet38: Int  // expected-error{{@IBOutlet property cannot have non-object type}}

  @IBOutlet var collection1b: [AnyObject]?
  @IBOutlet var collection1c: [AnyObject]!

  @IBOutlet var collection2b: [X]?
  @IBOutlet var collection2c: [X]!

  @IBOutlet var collection3b: [OP1]?
  @IBOutlet var collection3c: [OP1]!

  @IBOutlet var collection4a: [CP1] // expected-error{{@IBOutlet property cannot be an array of non-'@objc' protocol type}}
  @IBOutlet var collection4b: ([CP1])? // expected-error{{@IBOutlet property cannot be an array of non-'@objc' protocol type}}
  @IBOutlet var collection4c: ([CP1])! // expected-error{{@IBOutlet property cannot be an array of non-'@objc' protocol type}}

  @IBOutlet var collection5b: ([String])?  // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  @IBOutlet var collection5c: ([String])!  // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}

  @IBOutlet var collection6a: [NonObjC] // expected-error{{@IBOutlet property cannot be an array of non-'@objc' class type}}
  @IBOutlet var collection6b: ([NonObjC])? // expected-error{{@IBOutlet property cannot be an array of non-'@objc' class type}}
  @IBOutlet var collection6c: ([NonObjC])! // expected-error{{@IBOutlet property cannot be an array of non-'@objc' class type}}

  init() { }
}

@objc class Infer {
  @IBOutlet var outlet1: Infer!
  @IBOutlet weak var outlet2: Infer!

  func testOptionalNess() {
   let _ = outlet1!
   let _ = outlet2!
  }

  func testUnchecked() {
   let _ = outlet1
   let _ = outlet2
  }
  
  // This outlet is strong and optional.
  @IBOutlet var outlet4: AnyObject?

  func testStrong() {
    if outlet4 != nil {}
  }
}



@objc class C {
}

@objc protocol Proto {
}

class SwiftGizmo {
  @IBOutlet var a : C!
  @IBOutlet var b1 : [C]
  @IBOutlet var b2 : [C]!
  @IBOutlet var c : String!
  @IBOutlet var d : [String]! // expected-error{{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  @IBOutlet var e : Proto!

  @IBOutlet var f : C?
  @IBOutlet var g : C!

  @IBOutlet weak var h : C?
  @IBOutlet weak var i : C!
  @IBOutlet unowned var j : C // expected-error{{@IBOutlet property has non-optional type 'C'}}
  // expected-note @-1{{add '?' to form the optional type 'C?'}}{{30-30=?}}
  // expected-note @-2{{add '!' to form the implicitly unwrapped optional type 'C!'}}{{30-30=!}}
  @IBOutlet unowned(unsafe) var k : C // expected-error{{@IBOutlet property has non-optional type 'C'}}
  // expected-note @-1{{add '?' to form the optional type 'C?'}}{{38-38=?}}
  // expected-note @-2{{add '!' to form the implicitly unwrapped optional type 'C!'}}{{38-38=!}}

  @IBOutlet var bad1 : Int  // expected-error {{@IBOutlet property cannot have non-object type 'Int'}}

  @IBOutlet var dup: C! // expected-note{{'dup' previously declared here}}
  @IBOutlet var dup: C! // expected-error{{invalid redeclaration of 'dup'}}

  init() {}
}

class MissingOptional {
  @IBOutlet var a: C // expected-error{{@IBOutlet property has non-optional type 'C'}}
  // expected-note @-1{{add '?' to form the optional type 'C?'}}{{21-21=?}}
  // expected-note @-2{{add '!' to form the implicitly unwrapped optional type 'C!'}}{{21-21=!}}

  @IBOutlet weak var b: C // expected-error{{@IBOutlet property has non-optional type 'C'}}
  // expected-note @-1{{add '!' to form the implicitly unwrapped optional type 'C!'}}
  // expected-note @-2{{add '?' to form the optional type 'C?'}}

  init() {}
}

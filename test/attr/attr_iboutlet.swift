// RUN: %swift %s -verify

@IBOutlet // expected-error {{only instance properties can be declared 'IBOutlet'}}
var iboutlet_global: Int

@IBOutlet // expected-error {{only instance properties can be declared 'IBOutlet'}}
class IBOutletClassTy {}
@IBOutlet // expected-error {{only instance properties can be declared 'IBOutlet'}}
struct IBStructTy {}

@IBOutlet // expected-error {{only instance properties can be declared 'IBOutlet'}}
func IBFunction() -> () {}

@objc
class IBOutletWrapperTy {
  @IBOutlet
  var value : IBOutletWrapperTy = IBOutletWrapperTy() // no-warning

  @IBOutlet
  class var staticValue: IBOutletWrapperTy = 52  // expected-error {{cannot convert the expression's type 'Int' to type 'IBOutletWrapperTy'}}
  // expected-error@-2 {{only instance properties can be declared 'IBOutlet'}}
  // expected-error@-2 {{class variables not yet supported}}

  @IBOutlet // expected-error {{only instance properties can be declared 'IBOutlet'}}
  func click() -> () {}
}

struct S { }
enum E { }

protocol P1 { }
protocol P2 { }

@class_protocol protocol CP1 { }
@class_protocol protocol CP2 { }

@objc protocol OP1 { }
@objc protocol OP2 { }

class NonObjC {}

// Check where @IBOutlet can occur
@objc class X {
  // Class type
  @IBOutlet var outlet1: X
  @IBOutlet var outlet2: X?
  @IBOutlet var outlet3: X!

  @IBOutlet var outlet1a: NonObjC // expected-error{{'IBOutlet' property cannot have non-'@objc' class type 'NonObjC'}}
  @IBOutlet var outlet2a: NonObjC? // expected-error{{'IBOutlet' property cannot have non-'@objc' class type 'NonObjC'}}
  @IBOutlet var outlet3a: NonObjC! // expected-error{{'IBOutlet' property cannot have non-'@objc' class type 'NonObjC'}}

  // AnyObject
  @IBOutlet var outlet4: AnyObject
  @IBOutlet var outlet5: AnyObject?
  @IBOutlet var outlet6: AnyObject!

  // Protocol types
  @IBOutlet var outlet7: P1 // expected-error{{'IBOutlet' property cannot have non-object type 'P1'}}
  @IBOutlet var outlet8: CP1 // expected-error{{'IBOutlet' property cannot have non-object type 'CP1'}}
  @IBOutlet var outlet9: OP1
  @IBOutlet var outlet10: P1? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet11: CP1? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet12: OP1?
  @IBOutlet var outlet13: P1! // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet14: CP1! // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet15: OP1!

  // Class metatype
  @IBOutlet var outlet15b: X.Type // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet16: X.Type? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet17: X.Type! // expected-error{{'IBOutlet' property cannot have non-object type}}

  // AnyClass
  @IBOutlet var outlet18: AnyClass // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet19: AnyClass? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet20: AnyClass! // expected-error{{'IBOutlet' property cannot have non-object type}}

  // Protocol types
  @IBOutlet var outlet21: P1.Type // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet22: CP1.Type // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet23: OP1.Type // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet24: P1.Type? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet25: CP1.Type? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet26: OP1.Type? // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet27: P1.Type! // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet28: CP1.Type! // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet29: OP1.Type! // expected-error{{'IBOutlet' property cannot have non-object type}}
  
  // weak/unowned
  @IBOutlet weak var outlet30: X?
  @IBOutlet weak var outlet31: X!

  // String
  @IBOutlet var outlet32: String
  @IBOutlet var outlet33: String?
  @IBOutlet var outlet34: String!

  // Other bad cases
  @IBOutlet var outlet35: S // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet36: E // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet37: (X, X)  // expected-error{{'IBOutlet' property cannot have non-object type}}
  @IBOutlet var outlet38: Int  // expected-error{{'IBOutlet' property cannot have non-object type}}

  @IBOutlet var collection1a: AnyObject[]
  @IBOutlet var collection1b: (AnyObject[])?
  @IBOutlet var collection1c: (AnyObject[])!

  @IBOutlet var collection2a: X[]
  @IBOutlet var collection2b: (X[])?
  @IBOutlet var collection2c: (X[])!

  @IBOutlet var collection3a: OP1[]
  @IBOutlet var collection3b: (OP1[])?
  @IBOutlet var collection3c: (OP1[])!

  @IBOutlet var collection4a: CP1[] // expected-error{{'IBOutlet' property cannot be an array of non-object type}}
  @IBOutlet var collection4b: (CP1[])? // expected-error{{'IBOutlet' property cannot be an array of non-object type}}
  @IBOutlet var collection4c: (CP1[])! // expected-error{{'IBOutlet' property cannot be an array of non-object type}}

  @IBOutlet var collection5a: String[]    // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  @IBOutlet var collection5b: (String[])?  // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  @IBOutlet var collection5c: (String[])!  // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}

  @IBOutlet var collection6a: NonObjC[] // expected-error{{'IBOutlet' property cannot be an array of non-'@objc' class type}}
  @IBOutlet var collection6b: (NonObjC[])? // expected-error{{'IBOutlet' property cannot be an array of non-'@objc' class type}}
  @IBOutlet var collection6c: (NonObjC[])! // expected-error{{'IBOutlet' property cannot be an array of non-'@objc' class type}}

  init() { }
}

// Turn non-optional @IBOutlets into (possibly unchecked) optional ones.
@objc class Infer {
  @IBOutlet var outlet1: Infer
  @IBOutlet weak var outlet2: Infer

  func testOptionalNess() {
   let o1 = outlet1!
   let o2 = outlet2!
  }

  func testUnchecked() {
   let o1 = outlet1
   let o2 = outlet2
  }
}



@objc class C {
}

@objc protocol Proto {
}

class SwiftGizmo {
  @IBOutlet var a : C
  @IBOutlet var b : C[]
  @IBOutlet var c : String
  @IBOutlet var d : String[]  // expected-error {{property cannot be marked @IBOutlet because its type cannot be represented in Objective-C}}
  @IBOutlet var e : Proto

  @IBOutlet var f : C?
  @IBOutlet var g : C!

  @IBOutlet weak var h : C?
  @IBOutlet weak var i : C!
  @IBOutlet unowned var j : C


  @IBOutlet var bad1 : Int  // expected-error {{'IBOutlet' property cannot have non-object type 'Int'}}

  init() {}
}

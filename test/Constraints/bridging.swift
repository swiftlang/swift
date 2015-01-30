// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

import Foundation

class BridgedClass : NSObject, NSCopying { 
  func copyWithZone(zone: NSZone) -> AnyObject {
    return self
  }
}

class BridgedClassSub : BridgedClass { }

struct BridgedStruct : Hashable, _ObjectiveCBridgeable {
  var hashValue: Int { return 0 }

 static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return BridgedClass.self
  }

  func _bridgeToObjectiveC() -> BridgedClass {
    return BridgedClass()
  }

  static func _forceBridgeFromObjectiveC(
    x: BridgedClass, 
    inout result: BridgedStruct?) {
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: BridgedClass,
    inout result: BridgedStruct?
  ) -> Bool {
    return true
  }
}

func ==(x: BridgedStruct, y: BridgedStruct) -> Bool { return true }

struct NotBridgedStruct : Hashable { 
  var hashValue: Int { return 0 }
}

func ==(x: NotBridgedStruct, y: NotBridgedStruct) -> Bool { return true }

class OtherClass : Hashable { 
  var hashValue: Int { return 0 }
}
func ==(x: OtherClass, y: OtherClass) -> Bool { return true }

// Basic bridging
func bridgeToObjC(s: BridgedStruct) -> BridgedClass {
  return s
  return s as BridgedClass
}

func bridgeToAnyObject(s: BridgedStruct) -> AnyObject {
  return s
  return s as AnyObject
}

func bridgeFromObjC(c: BridgedClass) -> BridgedStruct {
  return c // expected-error{{'BridgedClass' is not convertible to 'BridgedStruct'}}
  return c as BridgedStruct
}

func bridgeFromObjCDerived(s: BridgedClassSub) -> BridgedStruct {
  return s // expected-error{{'BridgedClassSub' is not convertible to 'BridgedStruct'}}
  return s as BridgedStruct
}

// Array -> NSArray
func arrayToNSArray() {
  var nsa: NSArray

  nsa = [AnyObject]()
  nsa = [BridgedClass]()
  nsa = [OtherClass]()
  nsa = [BridgedStruct]()
  nsa = [NotBridgedStruct]() // expected-error{{cannot assign a value of type '[(NotBridgedStruct)]' to a value of type 'NSArray'}}

  nsa = [AnyObject]() as NSArray
  nsa = [BridgedClass]() as NSArray
  nsa = [OtherClass]() as NSArray
  nsa = [BridgedStruct]() as NSArray
  nsa = [NotBridgedStruct]() as NSArray // expected-error{{cannot assign to immutable value of type 'NSArray'}} expected-error{{'[(NotBridgedStruct)]' is not convertible to 'NSArray'}}
}

// NSArray -> Array
func nsArrayToArray(nsa: NSArray) {
  var arr1: [AnyObject] = nsa // expected-error{{'NSArray' is not convertible to '[AnyObject]'}}
  var arr2: [BridgedClass] = nsa // expected-error{{'NSArray' is not convertible to '[BridgedClass]'}}
  var arr3: [OtherClass] = nsa // expected-error{{'NSArray' is not convertible to '[OtherClass]'}}
  var arr4: [BridgedStruct] = nsa // expected-error{{'NSArray' is not convertible to '[BridgedStruct]'}}
  var arr5: [NotBridgedStruct] = nsa // expected-error{{'NSArray' is not convertible to '[NotBridgedStruct]'}}

  var arr1b: [AnyObject] = nsa as [AnyObject]
  var arr2b: [BridgedClass] = nsa as [BridgedClass] // expected-error{{'BridgedClass' is not identical to 'AnyObject'}}
  var arr3b: [OtherClass] = nsa as [OtherClass] // expected-error{{'OtherClass' is not identical to 'AnyObject'}}
  var arr4b: [BridgedStruct] = nsa as [BridgedStruct] // expected-error{{'BridgedStruct' is not identical to 'AnyObject'}}
  var arr5b: [NotBridgedStruct] = nsa as [NotBridgedStruct] // expected-error{{'NSArray' is not convertible to '[NotBridgedStruct]'}}

  var arr6: Array = nsa as Array
  arr6 = arr1
  arr1 = arr6
}

func dictionaryToNSDictionary() {
  // FIXME: These diagnostics are awful.

  var nsd: NSDictionary

  nsd = [NSObject : AnyObject]()
  nsd = [NSObject : AnyObject]() as NSDictionary
  nsd = [NSObject : BridgedClass]()
  nsd = [NSObject : BridgedClass]() as NSDictionary
  nsd = [NSObject : OtherClass]()
  nsd = [NSObject : OtherClass]() as NSDictionary
  nsd = [NSObject : BridgedStruct]()
  nsd = [NSObject : BridgedStruct]() as NSDictionary
  nsd = [NSObject : NotBridgedStruct]() // expected-error{{cannot assign a value of type '[NSObject : NotBridgedStruct]' to a value of type 'NSDictionary'}}
  nsd = [NSObject : NotBridgedStruct]() as NSDictionary // expected-error{{cannot assign to immutable value of type 'NSDictionary'}} expected-error{{'[NSObject : NotBridgedStruct]' is not convertible to 'NSDictionary'}}

  nsd = [NSObject : BridgedClass?]() // expected-error{{cannot assign a value of type '[NSObject : BridgedClass?]' to a value of type 'NSDictionary'}}
  nsd = [NSObject : BridgedClass?]() as NSDictionary // expected-error{{cannot assign to immutable value of type 'NSDictionary'}} expected-error{{'[NSObject : BridgedClass?]' is not convertible to 'NSDictionary'}}
  nsd = [NSObject : BridgedStruct?]()  // expected-error{{cannot assign a value of type '[NSObject : BridgedStruct?]' to a value of type 'NSDictionary'}}
  nsd = [NSObject : BridgedStruct?]() as NSDictionary // expected-error{{cannot assign to immutable value of type 'NSDictionary'}} expected-error{{'[NSObject : BridgedStruct?]' is not convertible to 'NSDictionary'}}

  nsd = [BridgedClass : AnyObject]()
  nsd = [BridgedClass : AnyObject]() as NSDictionary
  nsd = [OtherClass : AnyObject]()
  nsd = [OtherClass : AnyObject]() as NSDictionary
  nsd = [BridgedStruct : AnyObject]()
  nsd = [BridgedStruct : AnyObject]() as NSDictionary
  nsd = [NotBridgedStruct : AnyObject]()  // expected-error{{cannot assign a value of type '[NotBridgedStruct : AnyObject]' to a value of type 'NSDictionary'}}
  nsd = [NotBridgedStruct : AnyObject]() as NSDictionary  // expected-error{{cannot assign to immutable value of type 'NSDictionary'}} expected-error{{'[NotBridgedStruct : AnyObject]' is not convertible to 'NSDictionary'}}

  // <rdar://problem/17134986>
  var bcOpt: BridgedClass?
  nsd = [BridgedStruct() : bcOpt] // expected-error{{cannot assign a value of type '[BridgedStruct : BridgedClass?]' to a value of type 'NSDictionary'}}
}

// In this case, we should not implicitly convert Dictionary to NSDictionary.
struct NotEquatable {}
func notEquatableError(d: Dictionary<Int, NotEquatable>) -> Bool {
  // FIXME: Another awful diagnostic.
  return d == d // expected-error{{binary operator '==' cannot be applied to two Dictionary<Int, NotEquatable> operands}}
}

// NSString -> String
var nss1 = "Some great text" as NSString
var nss2: NSString = ((nss1 as String) + ", Some more text") as NSString

// <rdar://problem/17943223>
var inferDouble = 1.0/10
let d: Double = 3.14159
inferDouble = d

// rdar://problem/17962491
var inferDouble2 = 1 % 3 / 3.0
let d2: Double = 3.14159
inferDouble2 = d2

// rdar://problem/18269449
var i1: Int = 1.5 * 3.5 // expected-error{{'Double' is not convertible to 'Int'}}

// rdar://problem/18330319
func rdar18330319(s: String, d: [String : AnyObject]) {
  let t = d[s] as! String?
}

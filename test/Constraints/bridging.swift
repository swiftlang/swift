// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-swift-frontend %s -parse -verify

import Foundation

class BridgedClass : NSObject, NSCopying { 
  func copyWithZone(zone: NSZone) -> AnyObject! {
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

  static func _forceBridgeFromObjectiveC(x: BridgedClass) -> BridgedStruct {
    return BridgedStruct()
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: BridgedClass
  ) -> BridgedStruct? {
    return self._forceBridgeFromObjectiveC(x)
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
}

func bridgeToAnyObject(s: BridgedStruct) -> AnyObject {
  return s
}

func bridgeFromObjC(c: BridgedClass) -> BridgedStruct {
  return c
}

func bridgeFromObjCDerived(s: BridgedClassSub) -> BridgedStruct {
  return s
}

// Array -> NSArray
func arrayToNSArray() {
  var nsa: NSArray

  nsa = [AnyObject]()
  nsa = [BridgedClass]()
  nsa = [OtherClass]()
  nsa = [BridgedStruct]()
  nsa = [NotBridgedStruct]() // expected-error{{'[(NotBridgedStruct)]' is not convertible to 'NSArray'}}
}

// NSArray -> Array
func nsArrayToArray(nsa: NSArray) {
  var arr1: [AnyObject] = nsa
  let arr2: [BridgedClass] = nsa // expected-error{{'BridgedClass' is not identical to 'AnyObject'}}
  let arr3: [OtherClass] = nsa  // expected-error{{'OtherClass' is not identical to 'AnyObject'}}
  let arr4: [BridgedStruct] = nsa  // expected-error{{'BridgedStruct' is not identical to 'AnyObject'}}
  let arr5: [NotBridgedStruct] = nsa  // expected-error{{'NSArray' is not convertible to '[NotBridgedStruct]'}}

  var arr6: Array = nsa // infers [AnyObject].
  arr6 = arr1
  arr1 = arr6
}

func dictionaryToNSDictionary() {
  // FIXME: These diagnostics are awful.

  var nsd: NSDictionary

  nsd = [NSObject : AnyObject]()
  nsd = [NSObject : BridgedClass]()
  nsd = [NSObject : OtherClass]()
  nsd = [NSObject : BridgedStruct]()
  nsd = [NSObject : NotBridgedStruct]() // expected-error{{'[NSObject : NotBridgedStruct]' is not convertible to 'NSDictionary'}}

  nsd = [NSObject : BridgedClass?]() // expected-error{{'[NSObject : BridgedClass?]' is not convertible to 'NSDictionary'}}
  nsd = [NSObject : BridgedStruct?]()  // expected-error{{'[NSObject : BridgedStruct?]' is not convertible to 'NSDictionary'}}

  nsd = [BridgedClass : AnyObject]()
  nsd = [OtherClass : AnyObject]()
  nsd = [BridgedStruct : AnyObject]()
  nsd = [NotBridgedStruct : AnyObject]()  // expected-error{{'[NotBridgedStruct : AnyObject]' is not convertible to 'NSDictionary'}}

  // <rdar://problem/17134986>
  var bcOpt: BridgedClass?
  nsd = [BridgedStruct() : bcOpt] // expected-error{{cannot convert the expression's type '()' to type 'BridgedStruct'}}
}

// In this case, we should not implicitly convert Dictionary to NSDictionary.
struct NotEquatable {}
func notEquatableError(d: Dictionary<Int, NotEquatable>) -> Bool {
  // FIXME: Another awful diagnostic.
  return d == d // expected-error{{cannot invoke '==' with an argument list of type '(Dictionary<Int, NotEquatable>, Dictionary<Int, NotEquatable>)'}}
}

// NSString -> String
var nss1 = "Some great text" as NSString
var nss2: NSString = nss1 + ", Some more text"

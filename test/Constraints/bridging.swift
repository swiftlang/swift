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
  return c // expected-error{{cannot convert return expression of type 'BridgedClass' to return type 'BridgedStruct'}}
  return c as BridgedStruct
}

func bridgeFromObjCDerived(s: BridgedClassSub) -> BridgedStruct {
  return s // expected-error{{cannot convert return expression of type 'BridgedClassSub' to return type 'BridgedStruct'}}
  return s as BridgedStruct
}

// Array -> NSArray
func arrayToNSArray() {
  var nsa: NSArray

  nsa = [AnyObject]()
  nsa = [BridgedClass]()
  nsa = [OtherClass]()
  nsa = [BridgedStruct]()
  nsa = [NotBridgedStruct]() // expected-error{{cannot assign value of type '[NotBridgedStruct]' to type 'NSArray'}}

  nsa = [AnyObject]() as NSArray
  nsa = [BridgedClass]() as NSArray
  nsa = [OtherClass]() as NSArray
  nsa = [BridgedStruct]() as NSArray
  nsa = [NotBridgedStruct]() as NSArray // expected-error{{cannot convert value of type '[NotBridgedStruct]' to type 'NSArray' in coercion}}
  _ = nsa
}

// NSArray -> Array
func nsArrayToArray(nsa: NSArray) {
  var arr1: [AnyObject] = nsa // expected-error{{'NSArray' is not implicitly convertible to '[AnyObject]'; did you mean to use 'as' to explicitly convert?}} {{30-30= as [AnyObject]}}
  var _: [BridgedClass] = nsa // expected-error{{'NSArray' is not convertible to '[BridgedClass]'}} {{30-30= as! [BridgedClass]}}
  var _: [OtherClass] = nsa // expected-error{{'NSArray' is not convertible to '[OtherClass]'}} {{28-28= as! [OtherClass]}}
  var _: [BridgedStruct] = nsa // expected-error{{'NSArray' is not convertible to '[BridgedStruct]'}} {{31-31= as! [BridgedStruct]}}
  var _: [NotBridgedStruct] = nsa // expected-error{{cannot convert value of type 'NSArray' to specified type '[NotBridgedStruct]'}}

  var _: [AnyObject] = nsa as [AnyObject]
  var _: [BridgedClass] = nsa as [BridgedClass] // expected-error{{'NSArray' is not convertible to '[BridgedClass]'; did you mean to use 'as!' to force downcast?}} {{31-33=as!}}
  var _: [OtherClass] = nsa as [OtherClass] // expected-error{{'NSArray' is not convertible to '[OtherClass]'; did you mean to use 'as!' to force downcast?}} {{29-31=as!}}
  var _: [BridgedStruct] = nsa as [BridgedStruct] // expected-error{{'NSArray' is not convertible to '[BridgedStruct]'; did you mean to use 'as!' to force downcast?}} {{32-34=as!}}
  var _: [NotBridgedStruct] = nsa as [NotBridgedStruct] // expected-error{{cannot convert value of type 'NSArray' to type '[NotBridgedStruct]' in coercion}}

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
  nsd = [NSObject : NotBridgedStruct]() // expected-error{{cannot assign value of type '[NSObject : NotBridgedStruct]' to type 'NSDictionary'}}
  nsd = [NSObject : NotBridgedStruct]() as NSDictionary // expected-error{{cannot convert value of type '[NSObject : NotBridgedStruct]' to type 'NSDictionary' in coercion}}

  nsd = [NSObject : BridgedClass?]() // expected-error{{cannot assign value of type '[NSObject : BridgedClass?]' to type 'NSDictionary'}}
  nsd = [NSObject : BridgedClass?]() as NSDictionary // expected-error{{cannot convert value of type '[NSObject : BridgedClass?]' to type 'NSDictionary' in coercion}}
  nsd = [NSObject : BridgedStruct?]()  // expected-error{{cannot assign value of type '[NSObject : BridgedStruct?]' to type 'NSDictionary'}}
  nsd = [NSObject : BridgedStruct?]() as NSDictionary //expected-error{{cannot convert value of type '[NSObject : BridgedStruct?]' to type 'NSDictionary' in coercion}}

  nsd = [BridgedClass : AnyObject]()
  nsd = [BridgedClass : AnyObject]() as NSDictionary
  nsd = [OtherClass : AnyObject]()
  nsd = [OtherClass : AnyObject]() as NSDictionary
  nsd = [BridgedStruct : AnyObject]()
  nsd = [BridgedStruct : AnyObject]() as NSDictionary
  nsd = [NotBridgedStruct : AnyObject]()  // expected-error{{cannot assign value of type '[NotBridgedStruct : AnyObject]' to type 'NSDictionary'}}
  nsd = [NotBridgedStruct : AnyObject]() as NSDictionary  // expected-error{{cannot convert value of type '[NotBridgedStruct : AnyObject]' to type 'NSDictionary' in coercion}}

  // <rdar://problem/17134986>
  var bcOpt: BridgedClass?
  nsd = [BridgedStruct() : bcOpt] // expected-error{{value of optional type 'BridgedClass?' not unwrapped; did you mean to use '!' or '?'?}}
  bcOpt = nil
  _ = nsd
}

// In this case, we should not implicitly convert Dictionary to NSDictionary.
struct NotEquatable {}
func notEquatableError(d: Dictionary<Int, NotEquatable>) -> Bool {
  // FIXME: Another awful diagnostic.
  return d == d // expected-error{{binary operator '==' cannot be applied to two 'Dictionary<Int, NotEquatable>' operands}}
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: }}
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
var i1: Int = 1.5 * 3.5 // expected-error{{cannot convert value of type 'Double' to expected argument type 'Int'}}

// rdar://problem/18330319
func rdar18330319(s: String, d: [String : AnyObject]) {
  _ = d[s] as! String?
}

// rdar://problem/19551164
func rdar19551164a(s: String, _ a: [String]) {}
func rdar19551164b(s: NSString, _ a: NSArray) {
  rdar19551164a(s, a) // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{18-18= as String}}
  // expected-error@-1{{'NSArray' is not convertible to '[String]'; did you mean to use 'as!' to force downcast?}}{{21-21= as! [String]}}
}

// rdar://problem/19695671
func takesSet<T: Hashable>(p: Set<T>) {}
func takesDictionary<K: Hashable, V>(p: Dictionary<K, V>) {}
func takesArray<T>(t: Array<T>) {}
func rdar19695671() {
  takesSet(NSSet() as! Set) // expected-error{{cannot invoke 'takesSet' with an argument list of type '(Set<_>)'}}
  // expected-note @-1 {{expected an argument list of type '(Set<T>)'}}
  takesDictionary(NSDictionary() as! Dictionary) // expected-error{{cannot invoke 'takesDictionary' with an argument list of type '(Dictionary<_, _>)'}}
  // expected-note @-1 {{expected an argument list of type '(Dictionary<K, V>)'}}
  takesArray(NSArray() as! Array) // expected-error{{cannot invoke 'takesArray' with an argument list of type '(Array<_>)'}}
  // expected-note @-1 {{expected an argument list of type '(Array<T>)'}}
}


// This failed at one point while fixing rdar://problem/19600325.
func getArrayOfAnyObject(_: AnyObject) -> [AnyObject] { return [] }
func testCallback(f: (AnyObject) -> AnyObject?) {}
testCallback { return getArrayOfAnyObject($0) }

// <rdar://problem/19724719> Type checker thinks "(optionalNSString ?? nonoptionalNSString) as String" is a forced cast
func rdar19724719(f: (String) -> (), s1: NSString?, s2: NSString) {
  f((s1 ?? s2) as String)
}

// <rdar://problem/19770981>
func rdar19770981(s: String, ns: NSString) {
  func f(s: String) {}
  f(ns) // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{7-7= as String}}
  f(ns as String)
  // 'as' has higher precedence than '>' so no parens are necessary with the fixit:
  s > ns // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{9-9= as String}}
  _ = s > ns as String
  ns > s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{5-5= as String}}
  _ = ns as String > s

  // 'as' has lower precedence than '+' so add parens with the fixit:
  s + ns // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{7-7=(}}{{9-9= as String)}}
  _ = s + (ns as String)
  ns + s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{3-3=(}}{{5-5= as String)}}
  _ = (ns as String) + s
}

// <rdar://problem/19831919> Fixit offers as! conversions that are known to always fail
func rdar19831919() {
  var s1 = 1 + "str"; // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}} expected-note{{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String), (Int, UnsafeMutablePointer<Memory>), (Int, UnsafePointer<Memory>)}}
}

// <rdar://problem/19831698> Incorrect 'as' fixits offered for invalid literal expressions
func rdar19831698() {
  var v70 = true + 1 // expected-error{{binary operator '+' cannot be applied to operands of type 'Bool' and 'Int'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (UnsafeMutablePointer<Memory>, Int), (UnsafePointer<Memory>, Int)}}
  var v71 = true + 1.0 // expected-error{{cannot convert value of type 'Bool' to expected argument type 'Double'}}
  var v72 = true + true // expected-error{{binary operator '+' cannot be applied to two 'Bool' operands}}
  // expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists:}}
  var v73 = true + [] // expected-error{{binary operator '+' cannot be applied to operands of type 'Bool' and '[_]'}}
  // expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists:}}
  var v75 = true + "str" // expected-error{{cannot convert value of type 'Bool' to expected argument type 'String'}}
}

// <rdar://problem/19836341> Incorrect fixit for NSString? to String? conversions
func rdar19836341(ns: NSString?, vns: NSString?) {
  var vns = vns
  let _: String? = ns // expected-error{{cannot convert value of type 'NSString?' to specified type 'String?'}}
  var _: String? = ns // expected-error{{cannot convert value of type 'NSString?' to specified type 'String?'}}
  // FIXME: there should be a fixit appending "as String?" to the line; for now
  // it's sufficient that it doesn't suggest appending "as String"

  // Important part about below diagnostic is that from-type is described as
  // 'NSString?' and not '@lvalue NSString?':
  let _: String? = vns // expected-error{{cannot convert value of type 'NSString?' to specified type 'String?'}}
  var _: String? = vns // expected-error{{cannot convert value of type 'NSString?' to specified type 'String?'}}
  
  vns = ns
}

// <rdar://problem/20029786> Swift compiler sometimes suggests changing "as!" to "as?!"
func rdar20029786(ns: NSString?) {
  var s: String = ns ?? "str" as String as String // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{19-19=(}} {{50-50=) as String}}
  var s2 = ns ?? "str" as String as String

  let s3: NSString? = "str" as String?

  var s4: String = ns ?? "str" // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}{{20-20=(}}{{31-31=) as String}}
  var s5: String = (ns ?? "str") as String // fixed version
}

// <rdar://problem/19813772> QoI: Using as! instead of as in this case produces really bad diagnostic
func rdar19813772(nsma: NSMutableArray) {
  var a1 = nsma as! Array // expected-error{{generic parameter 'Element' could not be inferred}}
  // FIXME: The following diagnostic is misleading and should not happen: expected-warning@-1{{cast from 'NSMutableArray' to unrelated type 'Array<_>' always fails}}
  var a2 = nsma as! Array<AnyObject> // expected-warning{{forced cast from 'NSMutableArray' to 'Array<AnyObject>' always succeeds; did you mean to use 'as'?}} {{17-20=as}}
  var a3 = nsma as Array<AnyObject>
}


// <rdar://problem/20336036> QoI: Add cast-removing fixit for "Forced cast from 'T' to 'T' always succeeds"
func force_cast_fixit(a : [NSString]) -> [NSString] {
  return a as! [NSString] // expected-warning {{forced cast of '[NSString]' to same type has no effect}} {{12-27=}}
}

// <rdar://problem/21244068> QoI: IUO prevents specific diagnostic + fixit about non-implicitly converted bridge types
func rdar21244068(n: NSString!) -> String {
  return n  // expected-error {{'NSString!' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{11-11= as String}}
}


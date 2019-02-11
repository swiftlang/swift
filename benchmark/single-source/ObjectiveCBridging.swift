//===--- ObjectiveCBridging.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation

let t: [BenchmarkCategory] = [.validation, .bridging]
let ts: [BenchmarkCategory] = [.validation, .bridging, .String]

public let ObjectiveCBridging = [
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSString",
    runFunction: run_ObjectiveCBridgeFromNSString, tags: t,
    legacyFactor: 5),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSStringForced",
    runFunction: run_ObjectiveCBridgeFromNSStringForced, tags: t,
    legacyFactor: 5),
  BenchmarkInfo(name: "ObjectiveCBridgeToNSString",
    runFunction: run_ObjectiveCBridgeToNSString, tags: t),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSArrayAnyObject",
    runFunction: run_ObjectiveCBridgeFromNSArrayAnyObject, tags: t,
    legacyFactor: 100),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSArrayAnyObjectForced",
    runFunction: run_ObjectiveCBridgeFromNSArrayAnyObjectForced, tags: t,
    legacyFactor: 20),
  BenchmarkInfo(name: "ObjectiveCBridgeToNSArray",
    runFunction: run_ObjectiveCBridgeToNSArray, tags: t,
    legacyFactor: 50),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSArrayAnyObjectToString",
    runFunction: run_ObjectiveCBridgeFromNSArrayAnyObjectToString, tags: ts,
    legacyFactor: 100),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSArrayAnyObjectToStringForced",
    runFunction: run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced,
    tags: ts, legacyFactor: 200),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSDictionaryAnyObject",
    runFunction: run_ObjectiveCBridgeFromNSDictionaryAnyObject, tags: t,
    legacyFactor: 100),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSDictionaryAnyObjectForced",
    runFunction: run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced, tags: t,
    legacyFactor: 50),
  BenchmarkInfo(name: "ObjectiveCBridgeToNSDictionary",
    runFunction: run_ObjectiveCBridgeToNSDictionary, tags: t,
    legacyFactor: 50),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSDictionaryAnyObjectToString",
    runFunction: run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString,
    tags: ts, legacyFactor: 500),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced",
    runFunction: run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced,
    tags: ts, legacyFactor: 500),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSSetAnyObject",
    runFunction: run_ObjectiveCBridgeFromNSSetAnyObject, tags: t,
    legacyFactor: 200),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSSetAnyObjectForced",
    runFunction: run_ObjectiveCBridgeFromNSSetAnyObjectForced, tags: t,
    legacyFactor: 20),
  BenchmarkInfo(name: "ObjectiveCBridgeToNSSet",
    runFunction: run_ObjectiveCBridgeToNSSet, tags: t,
    legacyFactor: 50),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSSetAnyObjectToString",
    runFunction: run_ObjectiveCBridgeFromNSSetAnyObjectToString, tags: ts,
    legacyFactor: 500),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSSetAnyObjectToStringForced",
    runFunction: run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced, tags: ts,
    legacyFactor: 500),
  BenchmarkInfo(name: "ObjectiveCBridgeFromNSDateComponents",
    runFunction: run_ObjectiveCBridgeFromNSDateComponents, tags: t,
    setUpFunction: setup_dateComponents),
]

#if _runtime(_ObjC)
@inline(never)
public func forcedCast<NS, T>(_ ns: NS) -> T {
  return ns as! T
}

@inline(never)
public func conditionalCast<NS, T>(_ ns: NS) -> T? {
  return ns as? T
}
#endif


// === String === //

#if _runtime(_ObjC)
func createNSString() -> NSString {
  return NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
}

@inline(never)
func testObjectiveCBridgeFromNSString() {
  let nsString = createNSString()

  var s: String?
  for _ in 0 ..< 2_000 {
    // Call _conditionallyBridgeFromObjectiveC.
    let n : String? = conditionalCast(nsString)
    if n != nil {
      s = n!
    }
  }
  CheckResults(s != nil && s == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSString()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSStringForced() {
  let nsString = createNSString()

  var s: String?
  for _ in 0 ..< 2_000 {
    // Call _forceBridgeFromObjectiveC
    s = forcedCast(nsString)
  }
  CheckResults(s != nil && s == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSStringForced()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSString() {
  let nativeString = "Native"

  var s: NSString?
  for _ in 0 ..< 10_000 {
    // Call _BridgedToObjectiveC
    s = nativeString as NSString
  }
  CheckResults(s != nil && s == "Native")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeToNSString()
    }
  }
#endif
}

// === Array === //

#if _runtime(_ObjC)
func createNSArray() -> NSArray {
  let nsMutableArray = NSMutableArray()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  nsMutableArray.add(nsString)
  return nsMutableArray.copy() as! NSArray
}

@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObject() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 100 {
    if let nativeArray : [NSString] = conditionalCast(nsArray) {
       nativeString = forcedCast(nativeArray[0])
    }
  }
  CheckResults(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSArrayAnyObject()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectForced() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 500 {
    let nativeArray : [NSString] = forcedCast(nsArray)
    nativeString = forcedCast(nativeArray[0])
  }
  CheckResults(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSArrayAnyObjectForced()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSArray() {
  let nativeArray = ["abcde", "abcde", "abcde", "abcde", "abcde",
    "abcde", "abcde", "abcde", "abcde", "abcde"]

  var nsString : Any?
  for _ in 0 ..< 200 {
    let nsArray = nativeArray as NSArray
    nsString = nsArray.object(at: 0)
  }
  CheckResults(nsString != nil && (nsString! as! NSString).isEqual("abcde"))
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSArray(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeToNSArray()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectToString() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 100 {
    if let nativeArray : [String] = conditionalCast(nsArray) {
      nativeString = nativeArray[0]
    }
  }
  CheckResults(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSArrayAnyObjectToString()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSArrayAnyObjectToStringForced() {
  let nsArray = createNSArray()

  var nativeString : String?
  for _ in 0 ..< 50 {
    let nativeArray : [String] = forcedCast(nsArray)
    nativeString = nativeArray[0]
  }
  CheckResults(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSArrayAnyObjectToStringForced()
    }
  }
#endif
}

// === Dictionary === //

#if _runtime(_ObjC)
func createNSDictionary() -> NSDictionary {
  let nsMutableDictionary = NSMutableDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nsString2 = NSString(cString: "NSString that does not fit in tagged pointer 2", encoding: String.Encoding.utf8.rawValue)!
  let nsString3 = NSString(cString: "NSString that does not fit in tagged pointer 3", encoding: String.Encoding.utf8.rawValue)!
  let nsString4 = NSString(cString: "NSString that does not fit in tagged pointer 4", encoding: String.Encoding.utf8.rawValue)!
  let nsString5 = NSString(cString: "NSString that does not fit in tagged pointer 5", encoding: String.Encoding.utf8.rawValue)!
  let nsString6 = NSString(cString: "NSString that does not fit in tagged pointer 6", encoding: String.Encoding.utf8.rawValue)!
  let nsString7 = NSString(cString: "NSString that does not fit in tagged pointer 7", encoding: String.Encoding.utf8.rawValue)!
  let nsString8 = NSString(cString: "NSString that does not fit in tagged pointer 8", encoding: String.Encoding.utf8.rawValue)!
  let nsString9 = NSString(cString: "NSString that does not fit in tagged pointer 9", encoding: String.Encoding.utf8.rawValue)!
  let nsString10 = NSString(cString: "NSString that does not fit in tagged pointer 10", encoding: String.Encoding.utf8.rawValue)!
  nsMutableDictionary.setObject(1, forKey: nsString)
  nsMutableDictionary.setObject(2, forKey: nsString2)
  nsMutableDictionary.setObject(3, forKey: nsString3)
  nsMutableDictionary.setObject(4, forKey: nsString4)
  nsMutableDictionary.setObject(5, forKey: nsString5)
  nsMutableDictionary.setObject(6, forKey: nsString6)
  nsMutableDictionary.setObject(7, forKey: nsString7)
  nsMutableDictionary.setObject(8, forKey: nsString8)
  nsMutableDictionary.setObject(9, forKey: nsString9)
  nsMutableDictionary.setObject(10, forKey: nsString10)

  return nsMutableDictionary.copy() as! NSDictionary
}

@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObject() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var nativeInt : Int?
  for _ in 0 ..< 100 {
    if let nativeDictionary : [NSString : NSNumber] = conditionalCast(nsDictionary) {
       nativeInt = forcedCast(nativeDictionary[nsString])
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSDictionaryAnyObject()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectForced() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var nativeInt : Int?
  for _ in 0 ..< 200 {
    if let nativeDictionary : [NSString : NSNumber] = forcedCast(nsDictionary) {
       nativeInt = forcedCast(nativeDictionary[nsString])
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSDictionaryAnyObjectForced()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSDictionary() {
  let nativeDictionary = ["abcde1": 1, "abcde2": 2, "abcde3": 3, "abcde4": 4,
    "abcde5": 5, "abcde6": 6, "abcde7": 7, "abcde8": 8, "abcde9": 9,
    "abcde10": 10]
  let key = "abcde1" as NSString

  var nsNumber : Any?
  for _ in 0 ..< 200 {
    let nsDict = nativeDictionary as NSDictionary
    nsNumber = nsDict.object(forKey: key)
  }
  CheckResults(nsNumber != nil && (nsNumber as! Int) == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSDictionary(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeToNSDictionary()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectToString() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var nativeInt : Int?
  for _ in 0 ..< 20 {
    if let nativeDictionary : [String : Int] = conditionalCast(nsDictionary) {
       nativeInt = nativeDictionary[nativeString]
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSDictionaryAnyObjectToString()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced() {
  let nsDictionary = createNSDictionary()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var nativeInt : Int?
  for _ in 0 ..< 20 {
    if let nativeDictionary : [String : Int] = forcedCast(nsDictionary) {
       nativeInt = nativeDictionary[nativeString]
    }
  }
  CheckResults(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced()
    }
  }
#endif
}

// === Set === //

#if _runtime(_ObjC)
func createNSSet() -> NSSet {
  let nsMutableSet = NSMutableSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nsString2 = NSString(cString: "NSString that does not fit in tagged pointer 2", encoding: String.Encoding.utf8.rawValue)!
  let nsString3 = NSString(cString: "NSString that does not fit in tagged pointer 3", encoding: String.Encoding.utf8.rawValue)!
  let nsString4 = NSString(cString: "NSString that does not fit in tagged pointer 4", encoding: String.Encoding.utf8.rawValue)!
  let nsString5 = NSString(cString: "NSString that does not fit in tagged pointer 5", encoding: String.Encoding.utf8.rawValue)!
  let nsString6 = NSString(cString: "NSString that does not fit in tagged pointer 6", encoding: String.Encoding.utf8.rawValue)!
  let nsString7 = NSString(cString: "NSString that does not fit in tagged pointer 7", encoding: String.Encoding.utf8.rawValue)!
  let nsString8 = NSString(cString: "NSString that does not fit in tagged pointer 8", encoding: String.Encoding.utf8.rawValue)!
  let nsString9 = NSString(cString: "NSString that does not fit in tagged pointer 9", encoding: String.Encoding.utf8.rawValue)!
  let nsString10 = NSString(cString: "NSString that does not fit in tagged pointer 10", encoding: String.Encoding.utf8.rawValue)!
  nsMutableSet.add(nsString)
  nsMutableSet.add(nsString2)
  nsMutableSet.add(nsString3)
  nsMutableSet.add(nsString4)
  nsMutableSet.add(nsString5)
  nsMutableSet.add(nsString6)
  nsMutableSet.add(nsString7)
  nsMutableSet.add(nsString8)
  nsMutableSet.add(nsString9)
  nsMutableSet.add(nsString10)

  return nsMutableSet.copy() as! NSSet
}


@inline(never)
func testObjectiveCBridgeFromNSSetAnyObject() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var result : Bool?
  for _ in 0 ..< 50 {
    if let nativeSet : Set<NSString> = conditionalCast(nsSet) {
       result = nativeSet.contains(nsString)
    }
  }
  CheckResults(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObject(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSSetAnyObject()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectForced() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!

  var result : Bool?
  for _ in 0 ..< 500 {
    if let nativeSet : Set<NSString> = forcedCast(nsSet) {
       result = nativeSet.contains(nsString)
    }
  }
  CheckResults(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSSetAnyObjectForced()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeToNSSet() {
  let nativeSet = Set<String>(["abcde1", "abcde2", "abcde3", "abcde4", "abcde5",
      "abcde6", "abcde7", "abcde8", "abcde9", "abcde10"])
  let key = "abcde1" as NSString

  var nsString : Any?
  for _ in 0 ..< 200 {
    let nsDict = nativeSet as NSSet
    nsString = nsDict.member(key)
  }
  CheckResults(nsString != nil && (nsString as! String) == "abcde1")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSSet(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeToNSSet()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectToString() {
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String
  let nsSet = createNSSet()

  var result : Bool?
  for _ in 0 ..< 20 {
    if let nativeSet : Set<String> = conditionalCast(nsSet) {
       result = nativeSet.contains(nativeString)
    }
  }
  CheckResults(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSSetAnyObjectToString()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeFromNSSetAnyObjectToStringForced() {
  let nsSet = createNSSet()
  let nsString = NSString(cString: "NSString that does not fit in tagged pointer", encoding: String.Encoding.utf8.rawValue)!
  let nativeString = nsString as String

  var result : Bool?
  for _ in 0 ..< 20 {
    if let nativeSet : Set<String> = forcedCast(nsSet) {
       result = nativeSet.contains(nativeString)
    }
  }
  CheckResults(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
    testObjectiveCBridgeFromNSSetAnyObjectToStringForced()
    }
  }
#endif
}

#if _runtime(_ObjC)

//translated directly from an objc part of the original testcase
@objc class DictionaryContainer : NSObject {
  @objc var _dictionary:NSDictionary

  //simulate an objc property being imported via bridging
  @objc var dictionary:Dictionary<DateComponents, String> {
    @inline(never)
    get {
      return _dictionary as! Dictionary<DateComponents, String>
    }
  }

  override init() {
    _dictionary = NSDictionary()
    super.init()
    let dict = NSMutableDictionary()
    let calendar = NSCalendar(calendarIdentifier: .gregorian)!
    let now = Date()
    for day in 0..<36 {
      let date = calendar.date(byAdding: .day, value: -day, to: now, options: NSCalendar.Options())
      let components = calendar.components([.year, .month, .day], from: date!)
      dict[components as NSDateComponents] = "TESTING"
    }
    _dictionary = NSDictionary(dictionary: dict)
  }
}

var componentsContainer: DictionaryContainer? = nil
var componentsArray:[DateComponents]? = nil
#endif

public func setup_dateComponents() {
  #if _runtime(_ObjC)
  componentsContainer = DictionaryContainer()
  let now = Date()
  let calendar = Calendar(identifier: .gregorian)
  componentsArray = (0..<10).compactMap { (day) -> DateComponents? in
    guard let date = calendar.date(byAdding: .day, value: -day, to: now) else {
      return nil
    }

    return calendar.dateComponents([.year, .month, .day], from: date)
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeFromNSDateComponents(_ N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    for components in componentsArray! {
      let _ = componentsContainer!.dictionary[components]
    }
  }
  #endif
}

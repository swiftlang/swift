//===--- ObjectiveCBridging.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
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

public let benchmarks = [
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
  BenchmarkInfo(name: "ObjectiveCBridgeASCIIStringFromFile",
                runFunction: run_ASCIIStringFromFile, tags: ts,
                setUpFunction: setup_ASCIIStringFromFile),
  BenchmarkInfo(name: "UnicodeStringFromCodable",
                 runFunction: run_UnicodeStringFromCodable, tags: ts,
                 setUpFunction: setup_UnicodeStringFromCodable),
  BenchmarkInfo(name: "NSArray.bridged.objectAtIndex",
                  runFunction: run_BridgedNSArrayObjectAtIndex, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSArray.bridged.mutableCopy.objectAtIndex",
                  runFunction: run_BridgedNSArrayMutableCopyObjectAtIndex, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSArray.nonbridged.objectAtIndex",
                  runFunction: run_RealNSArrayObjectAtIndex, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSArray.nonbridged.mutableCopy.objectAtIndex",
                  runFunction: run_RealNSArrayMutableCopyObjectAtIndex, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSArray.bridged.bufferAccess",
                  runFunction: run_BridgedNSArrayBufferAccess, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSArray.bridged.repeatedBufferAccess",
                  runFunction: run_BridgedNSArrayRepeatedBufferAccess, tags: t,
                  setUpFunction: setup_bridgedArrays),
  BenchmarkInfo(name: "NSDictionary.bridged.enumerate",
                  runFunction: run_BridgedNSDictionaryEnumerate, tags: t,
                  setUpFunction: setup_bridgedDictionaries),
  BenchmarkInfo(name: "NSString.bridged.byteCount.ascii.ascii",
                  runFunction: run_BridgedNSStringLengthASCII_ASCII, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.byteCount.ascii.utf8",
                  runFunction: run_BridgedNSStringLengthASCII_UTF8, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.byteCount.ascii.utf16",
                  runFunction: run_BridgedNSStringLengthASCII_UTF16, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.byteCount.ascii.macroman",
                  runFunction: run_BridgedNSStringLengthASCII_MacRoman, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.byteCount.utf8.utf8",
                  runFunction: run_BridgedNSStringLengthUTF8_UTF8, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.byteCount.utf8.utf16",
                  runFunction: run_BridgedNSStringLengthUTF8_UTF16, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.ascii.ascii",
                  runFunction: run_BridgedNSStringMaxLengthASCII_ASCII, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.ascii.utf8",
                  runFunction: run_BridgedNSStringMaxLengthASCII_UTF8, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.ascii.utf16",
                  runFunction: run_BridgedNSStringMaxLengthASCII_UTF16, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.ascii.macroman",
                  runFunction: run_BridgedNSStringMaxLengthASCII_MacRoman, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.utf8.utf8",
                  runFunction: run_BridgedNSStringMaxLengthUTF8_UTF8, tags: ts,
                  setUpFunction: setup_bridgedStrings),
  BenchmarkInfo(name: "NSString.bridged.maxByteCount.utf8.utf16",
                  runFunction: run_BridgedNSStringMaxLengthUTF8_UTF16, tags: ts,
                  setUpFunction: setup_bridgedStrings),
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
  check(s != nil && s == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(s != nil && s == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSStringForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(s != nil && s == "Native")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObject(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeString != nil && nativeString! == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nsString != nil && (nsString! as! NSString).isEqual("abcde"))
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSArray(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeString != nil && nativeString == "NSString that does not fit in tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObject(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nsNumber != nil && (nsNumber as! Int) == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSDictionary(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nativeInt != nil && nativeInt == 1)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObject(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(nsString != nil && (nsString as! String) == "abcde1")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeToNSSet(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  check(result != nil && result!)
}
#endif

@inline(never)
public func run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
public func run_ObjectiveCBridgeFromNSDateComponents(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    for components in componentsArray! {
      let _ = componentsContainer!.dictionary[components]
    }
  }
  #endif
}

var asciiStringFromFile: String? = nil
public func setup_ASCIIStringFromFile() {
  #if _runtime(_ObjC)
  let url:URL
  if #available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *) {
    url = FileManager.default.temporaryDirectory.appendingPathComponent(
      "sphinx.txt"
    )
  } else {
    url = URL(fileURLWithPath: "/tmp/sphinx.txt")
  }
  var str = "Sphinx of black quartz judge my vow"
  str = Array(repeating: str, count: 100).joined()
  try? str.write(
    to: url,
    atomically: true,
    encoding: .ascii
  )
  asciiStringFromFile = try! String(contentsOf: url, encoding: .ascii)
  #endif
}

@inline(never)
public func run_ASCIIStringFromFile(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    blackHole((asciiStringFromFile! + "").utf8.count)
  }
  #endif
}

var unicodeStringFromCodable:String? = nil
var unicodeStringFromCodableDict = [String:Void]()
public func setup_UnicodeStringFromCodable() {
  do {
    let jsonString = "[\(String(reflecting: "Nice string which works rather slöw."))]"
    
    let decoded = try JSONDecoder().decode([String].self, from: Data(jsonString.utf8))
    let reEncoded = try JSONEncoder().encode(decoded)
    let desc = try JSONDecoder().decode([String].self, from: reEncoded)
    
    unicodeStringFromCodable = desc[0]
  } catch (_) {
    
  }
}

@inline(never)
public func run_UnicodeStringFromCodable(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    for _ in 0..<100 {
      unicodeStringFromCodableDict[identity(unicodeStringFromCodable!)] = ()
    }
  }
  #endif
}

#if _runtime(_ObjC)
var bridgedArray:NSArray! = nil
var bridgedDictionaryOfNumbersToNumbers:NSDictionary! = nil
var bridgedArrayMutableCopy:NSMutableArray! = nil
var nsArray:NSArray! = nil
var nsArrayMutableCopy:NSMutableArray! = nil
var bridgedASCIIString:NSString! = nil
var bridgedUTF8String:NSString! = nil
#endif

public func setup_bridgedArrays() {
  #if _runtime(_ObjC)
  var arr = Array(repeating: NSObject(), count: 100) as [AnyObject]
  bridgedArray = arr as NSArray
  bridgedArrayMutableCopy = (bridgedArray.mutableCopy() as! NSMutableArray)

  nsArray = NSArray(objects: &arr, count: 100)
  nsArrayMutableCopy = (nsArray.mutableCopy() as! NSMutableArray)
  #endif
}

public func setup_bridgedDictionaries() {
  var numDict = Dictionary<Int, Int>()
  for i in 0 ..< 100 {
    numDict[i] = i
  }
  bridgedDictionaryOfNumbersToNumbers = numDict as NSDictionary
}

public func setup_bridgedStrings() {
  #if _runtime(_ObjC)
  let str = Array(repeating: "The quick brown fox jumps over the lazy dog.", count: 100).joined()
  bridgedASCIIString = str as NSString
  let str2 = Array(repeating: "The quick brown fox jumps over the lazy dög.", count: 100).joined()
  bridgedUTF8String = str2 as NSString
  #endif
}

@inline(never)
public func run_BridgedNSArrayObjectAtIndex(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n * 50 {
    for i in 0..<100 {
      blackHole(bridgedArray[i])
    }
  }
  #endif
}

private func dictionaryApplier(
  _ keyPtr: UnsafeRawPointer?,
  _ valuePtr :UnsafeRawPointer?,
  _ contextPtr: UnsafeMutableRawPointer?
) -> Void {}

@inline(never)
public func run_BridgedNSDictionaryEnumerate(_ n: Int) {
  #if _runtime(_ObjC)
  let cf = bridgedDictionaryOfNumbersToNumbers as CFDictionary
  for _ in 0 ..< n * 50 {
    // Use CF to prevent Swift from providing an override, forcing going through ObjC bridging
    CFDictionaryApplyFunction(cf, dictionaryApplier, nil)
  }
  #endif
}

@inline(never)
public func run_BridgedNSArrayBufferAccess(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    for i in 0..<1000 {
      let tmp = nsArray as! [NSObject]
      blackHole(tmp)
      blackHole(tmp.withContiguousStorageIfAvailable {
        $0[0]
      })
    }
  }
  #endif
}

@inline(never)
public func run_BridgedNSArrayRepeatedBufferAccess(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    let tmp = nsArray as! [NSObject]
    blackHole(tmp)
    for i in 0..<1000 {
      blackHole(tmp.withContiguousStorageIfAvailable {
        $0[0]
      })
    }
  }
  #endif
}

@inline(never)
public func run_BridgedNSArrayMutableCopyObjectAtIndex(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n * 100 {
    for i in 0..<100 {
      blackHole(bridgedArrayMutableCopy[i])
    }
  }
  #endif
}

@inline(never)
public func run_RealNSArrayObjectAtIndex(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n * 100 {
    for i in 0..<100 {
      blackHole(nsArray[i])
    }
  }
  #endif
}

@inline(never)
public func run_RealNSArrayMutableCopyObjectAtIndex(_ n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n * 100 {
    for i in 0..<100 {
      blackHole(nsArrayMutableCopy[i])
    }
  }
  #endif
}

@inline(__always)
fileprivate func run_BridgedNSStringLength(_ asciiBase: Bool, _ enc: UInt, _ n: Int) {
  let str = asciiBase ? bridgedASCIIString : bridgedUTF8String
  for _ in 0 ..< n * 100 {
    blackHole(str!.lengthOfBytes(using: enc))
  }
}

@inline(never)
public func run_BridgedNSStringLengthASCII_ASCII(_ n: Int) {
  run_BridgedNSStringLength(true, 1 /* NSASCIIStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringLengthASCII_UTF8(_ n: Int) {
  run_BridgedNSStringLength(true, 4 /* NSUTF8StringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringLengthASCII_UTF16(_ n: Int) {
  run_BridgedNSStringLength(true, 10 /* NSUnicodeStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringLengthASCII_MacRoman(_ n: Int) {
  run_BridgedNSStringLength(true, 30 /* NSMacOSRomanStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringLengthUTF8_UTF8(_ n: Int) {
  run_BridgedNSStringLength(false, 4 /* NSUTF8StringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringLengthUTF8_UTF16(_ n: Int) {
  run_BridgedNSStringLength(false, 10 /* NSUnicodeStringEncoding */, n)
}

@inline(__always)
fileprivate func run_BridgedNSStringMaxLength(_ asciiBase: Bool, _ enc: UInt, _ n: Int) {
  let str = asciiBase ? bridgedASCIIString! : bridgedUTF8String!
  for _ in 0 ..< n * 100 {
    for i in 0..<100 {
      blackHole(str.maximumLengthOfBytes(using: enc))
    }
  }
}

@inline(never)
public func run_BridgedNSStringMaxLengthASCII_ASCII(_ n: Int) {
  run_BridgedNSStringMaxLength(true, 1 /* NSASCIIStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringMaxLengthASCII_UTF8(_ n: Int) {
  run_BridgedNSStringMaxLength(true, 4 /* NSUTF8StringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringMaxLengthASCII_UTF16(_ n: Int) {
  run_BridgedNSStringMaxLength(true, 10 /* NSUnicodeStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringMaxLengthASCII_MacRoman(_ n: Int) {
  run_BridgedNSStringMaxLength(true, 30 /* NSMacOSRomanStringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringMaxLengthUTF8_UTF8(_ n: Int) {
  run_BridgedNSStringMaxLength(false, 4 /* NSUTF8StringEncoding */, n)
}

@inline(never)
public func run_BridgedNSStringMaxLengthUTF8_UTF16(_ n: Int) {
  run_BridgedNSStringMaxLength(false, 10 /* NSUnicodeStringEncoding */, n)
}


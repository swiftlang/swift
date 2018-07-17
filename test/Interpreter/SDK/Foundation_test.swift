// RUN: %target-run-simple-swift-swift3
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest


// rdar://problem/18884272
// Make sure that NSObject conforms to NSObjectProtocol. This
// particular bug is ridiculously hard to trigger without a complete
// SDK, so it sits here.
let objcProtocol: NSObjectProtocol = NSObject()

var FoundationTestSuite = TestSuite("Foundation")

func asNSString(_ s: String) -> NSString { return s as NSString }
func asString(_ ns: NSString) -> String { return ns as String }

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSString") {
  var str = "Hello"
  var nsStr = str as NSString
  assert(nsStr.compare(str).rawValue == ComparisonResult.orderedSame.rawValue)
  assert(nsStr.compare(str) == ComparisonResult.orderedSame)
  nsStr = "World"
  str = nsStr as String
  // FIXME: Shouldn't need coercion here to resolve ambiguity. <rdar://problem/14637688>
  assert(str == asString(nsStr))
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSNumber") {
  var i = 17
  var d = 3.14159
  var b = true

  // Implicit boxing/explicit unboxing
  var nsNum: NSNumber = i as NSNumber
  expectEqual(i, Int(nsNum))

  nsNum = d as NSNumber
  expectEqual(d, Double(nsNum))

  nsNum = b as NSNumber
  expectEqual(b, Bool(nsNum))

  // Literals
  nsNum = 42
  expectEqual(42, Int(nsNum))

  nsNum = 3.14159
  expectEqual(3.14159, Double(nsNum))

  nsNum = false
  expectEqual(false, Bool(nsNum))
}

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSArray") {
  // Literals
  var nsArr: NSArray = [1, 2.5, "Hello"]
  assert(nsArr.count == 3)

  // Subscripting
  expectEqual(1, Int(nsArr[0] as! NSNumber))
  expectEqual(2.5, Double(nsArr[1] as! NSNumber))
  expectTrue((nsArr[2] as! NSString).isEqual("Hello"))

  // Iteration
  var result = [String]()
  for x: Any in nsArr {
    result.append((x as! NSObject).description)
  }
  expectEqualSequence(["1", "2.5", "Hello"], result)
}

FoundationTestSuite.test("NSMutableArray") {
  let nsMutableArr: NSMutableArray = ["Constant", "Moon"]
  nsMutableArr[0] = "Inconstant"

  expectEqual(2, nsMutableArr.count)
  expectEqual("Inconstant", nsMutableArr[0] as! NSString)
  expectEqual("Moon", nsMutableArr[1] as! NSString)
}

FoundationTestSuite.test("NSArrayVariadicInit") {
  let variadicArray = NSArray(objects: "A", "B", "C")
  expectEqual(3, variadicArray.count)
}

FoundationTestSuite.test("arrayConversions") {
  var nsa = NSArray()
  var aoa: Array<AnyObject> = []

  nsa as Array<AnyObject>
  var nsa2 = NSArray()
  var aoa2 = nsa2 as Array<AnyObject>

  var nsaoa = aoa as NSArray

  func nsArrayToAnyObjectArray(_ nsa: NSArray) -> [AnyObject] {
    return nsa as [AnyObject]
  }

  nsArrayToAnyObjectArray(nsa)
  nsArrayToAnyObjectArray(aoa as NSArray)
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSDictionary") {
  var nsDict : NSDictionary = [1 : "Hello", 2 : "World"]
  assert((nsDict[1]! as! NSString).isEqual("Hello"))
  assert((nsDict[2]! as! NSString).isEqual("World"))

  let nsMutableDict: NSMutableDictionary = ["Hello" : 1, "World" : 2 as NSNumber]
  assert((nsMutableDict["Hello"]! as AnyObject).isEqual(1))
  assert((nsMutableDict["World"]! as AnyObject).isEqual(2))
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSRange") {
  let nsRange = NSRange(1..<5)
  expectEqual("{1, 4}", String(NSStringFromRange(nsRange)))
}

FoundationTestSuite.test("RangeConversion") {
  let i: Int8 = 10
  let j: Int8 = 20

  let nsrFromInt8 = NSRange(i..<j)
  expectEqual(nsrFromInt8, NSRange(location: 10, length: 10))

  var r = Range(nsrFromInt8)
  expectNotNil(r)
  expectEqual(r!.lowerBound, 10)
  expectEqual(r!.upperBound, 20)
  expectType(Optional<Range<Int>>.self, &r)

  var r8 = Range<Int8>(nsrFromInt8)
  expectNotNil(r8 != nil)
  expectEqual(r8?.lowerBound, 10)
  expectEqual(r8?.upperBound, 20)
  expectType(Optional<Range<Int8>>.self, &r8)
  
  var nsrFromPartial = NSRange(..<5)
  expectEqual("{0, 5}", NSStringFromRange(nsrFromPartial))

  let s = "Hello, 🌎!"
  let b = s.firstIndex(of: ",")!
  let e = s.firstIndex(of: "!")!
  let nsr = NSRange(b..<e, in: s)
  expectEqual(nsr.location, 5)
  expectEqual(nsr.length, 4)
  let rs = Range(nsr, in: s)!
  expectEqual(s[rs], ", 🌎")

  let nsrTo = NSRange(..<b, in: s)
  expectEqual(nsrTo.location, 0)
  expectEqual(nsrTo.length, 5)
  let nsrFrom = NSRange(b..., in: s)
  expectEqual(nsrFrom.location,5)
  expectEqual(nsrFrom.length, 5)
  
  expectNil(Range(NSRange(location: 100, length: 0), in: s))
  expectNil(Range(NSRange(location: 0, length: 100), in: s))
  
  let empty = ""
  expectNil(Range(NSRange(location: 1, length: 0), in: empty))  
  expectNil(Range(NSRange(location: 0, length: 1), in: empty))
  expectNotNil(Range(NSRange(location: 0, length: 0), in: empty))

  // FIXME: enable once indices conform to RangeExpression
  // let nsrFull = NSRange(s.indices, in: s)
  // expectEqual(nsrFull.location, 0)
  // expectEqual(nsrFull.length, 10)
}

//===----------------------------------------------------------------------===//
// URLs
//===----------------------------------------------------------------------===//

FoundationTestSuite.test("NSURL") {
  let nsURL = NSURL(string: "http://llvm.org")!
  expectEqual("http://llvm.org", nsURL.description)
}

//===----------------------------------------------------------------------===//
// Pattern-matching
//===----------------------------------------------------------------------===//

func matchesEither(_ input: NSNumber, _ a: NSNumber, _ b: NSNumber) -> Bool {
  switch input {
  case a, b:
    return true
  default:
    return false
  }
}

FoundationTestSuite.test("patternMatching") {
  var one, two, three, oneAgain : NSNumber
  one = NSNumber(value: 1)
  two = NSNumber(value: 2)
  three = NSNumber(value: 3)
  oneAgain = NSNumber(value: 1)
  expectFalse(matchesEither(one, two, three))
  expectTrue(matchesEither(one, oneAgain, three))
  expectTrue(matchesEither(one, two, oneAgain))
}

//===----------------------------------------------------------------------===//
// Miscellaneous
//===----------------------------------------------------------------------===//

// <rdar://problem/14474701>
// Type checker used to crash on this.
class ClassWithDtor : NSObject {
  deinit {
    let noteCenter = NotificationCenter.default
    noteCenter.removeObserver(self, name: Notification.Name(rawValue: "ReceivedContentNotification"), object: nil)
  }
}

FoundationTestSuite.test("rdar://17584531") {
  // <rdar://problem/17584531>
  // Type checker used to be confused by this.
  var dict: NSDictionary = ["status": 200, "people": [["id": 255, "name": ["first": "John", "last": "Appleseed"] as NSDictionary] as NSDictionary] as NSArray]
  var dict2 = dict["people"].flatMap { $0 as? NSArray }?[0] as! NSDictionary
  expectEqual("Optional(255)", String(describing: dict2["id" as NSString]))
}

FoundationTestSuite.test("DarwinBoolean smoke test") {
  expectFalse(CFEqual("abc" as NSString as CFString, "def" as NSString as CFString))

  let _: CFArrayEqualCallBack = { DarwinBoolean($0 == $1) }
}

#if os(macOS)
FoundationTestSuite.test("NSRectEdge/constants") {
  // Check that the following constants have the correct type and value.
  //
  // It is valid to hardcode the value in the test.  The way they are currently
  // defined in the SDK makes them ABI for Objective-C code.
  expectEqual(0, CGRectEdge(rectEdge: NSRectEdge.minX).rawValue)
  expectEqual(0, NSRectEdge(rectEdge: CGRectEdge.minXEdge).rawValue)

  expectEqual(1, CGRectEdge(rectEdge: NSRectEdge.minY).rawValue)
  expectEqual(1, NSRectEdge(rectEdge: CGRectEdge.minYEdge).rawValue)

  expectEqual(2, CGRectEdge(rectEdge: NSRectEdge.maxX).rawValue)
  expectEqual(2, NSRectEdge(rectEdge: CGRectEdge.maxXEdge).rawValue)

  expectEqual(3, CGRectEdge(rectEdge: NSRectEdge.maxY).rawValue)
  expectEqual(3, NSRectEdge(rectEdge: CGRectEdge.maxYEdge).rawValue)
}
#endif


if #available(OSX 10.11, iOS 9.0, *) {
  FoundationTestSuite.test("UndoManager/ObjCClass") {
    let UM = UndoManager()

    // Confirm with an ObjC class.
    class ObjCClass : NSObject {
      var someProperty: String = ""
    }
    let f = ObjCClass()
    UM.registerUndo(withTarget: f) { target in
      target.someProperty = "expected"
    }
    UM.undo()
    expectEqual(f.someProperty, "expected")
  }

  FoundationTestSuite.test("UndoManager/SwiftClass") {
    let UM = UndoManager()

    // Confirm with a Swift class.
    class SwiftClass {
      var someOtherProperty: String = ""
    }
    var b = SwiftClass()
    UM.registerUndo(withTarget:b) { target in
      target.someOtherProperty = "expected"
    }
    UM.undo()
    expectEqual(b.someOtherProperty, "expected")
  }
}

private let KEY = "some-key"
private func createTestArchive() -> NSData {
  let mutableData = NSMutableData()
  let KA = NSKeyedArchiver(forWritingWith: mutableData)

  // Set up some fake data.
  let obj =  NSPredicate(value: true)
  KA.encode(obj, forKey: KEY)
  KA.encode(obj, forKey: NSKeyedArchiveRootObjectKey)
  KA.finishEncoding()

  return mutableData
}

FoundationTestSuite.test("NSKeyedUnarchiver/decodeObjectOfClass(_:forKey:)") {
  let obj = NSPredicate(value: true)
  let data = createTestArchive()

  var KU = NSKeyedUnarchiver(forReadingWith: data as Data)

  var missing = KU.decodeObject(of: NSPredicate.self, forKey: "Not there")
  expectNil(missing)
  expectType((NSPredicate?).self, &missing)

  var decoded = KU.decodeObject(of: NSPredicate.self, forKey: KEY)
  expectNotNil(decoded)
  expectType((NSPredicate?).self, &decoded)

  var wrongType = KU.decodeObject(of: DateFormatter.self, forKey: KEY)
  expectNil(missing)

  KU.finishDecoding()
}

if #available(OSX 10.11, iOS 9.0, *) {
  FoundationTestSuite.test("NSKeyedUnarchiver/decodeTopLevel*") {
    let obj = NSPredicate(value: true)
    let data = createTestArchive()

    // first confirm .decodeObjectWithClasses overlay requires an array of classes
    var KU = NSKeyedUnarchiver(forReadingWith: data as Data)
    var nonTopLevelResult = KU.decodeObject(of: [NSPredicate.self], forKey: KEY)
    expectTrue(nonTopLevelResult != nil)
    KU.finishDecoding()

    KU = NSKeyedUnarchiver(forReadingWith: data as Data)
    do {
      // decodeObjectForKey(_:) throws
      let decoded1 = try KU.decodeTopLevelObject(forKey: KEY) as? NSPredicate
      let missing1 = try KU.decodeTopLevelObject(forKey:"Not there")
      expectTrue(decoded1 != nil)
      expectEqual(obj, decoded1)
      expectTrue(missing1 == nil)
      KU.finishDecoding()

      // recreate so we can do the rest securely
      KU = NSKeyedUnarchiver(forReadingWith: data as Data)
      KU.requiresSecureCoding = true

      // decodeObjectOfClass(_:,forKey:) throws
      let decoded2 = try KU.decodeTopLevelObject(of: NSPredicate.self, forKey: KEY)
      let missing2 = try KU.decodeTopLevelObject(of: NSPredicate.self, forKey: "Not there")
      expectTrue(decoded2 != nil)
      expectEqual(obj, decoded2)
      expectTrue(missing2 == nil)

      let classes: [AnyClass] = [NSPredicate.self]
      let decoded3 = try KU.decodeTopLevelObject(of: classes, forKey: KEY) as? NSPredicate
      let missing3 = try KU.decodeTopLevelObject(of: classes, forKey: "Not there")
      expectTrue(decoded3 != nil)
      expectEqual(obj, decoded3)
      expectTrue(missing3 == nil)
    }
    catch {
      expectTrue(false) // should have no errors
    }
    KU.finishDecoding()

    KU = NSKeyedUnarchiver(forReadingWith: data as Data)
    KU.requiresSecureCoding = true
    do {
      // recreate so we avoid caches from above
      let wrong = try KU.decodeTopLevelObject(of: DateFormatter.self, forKey: KEY)
      expectUnreachable() // should have error
    }
    catch {
      // expected
    }
    KU.finishDecoding()
    KU = NSKeyedUnarchiver(forReadingWith: data as Data)
    KU.requiresSecureCoding = true
    do {
      let wrongClasses: [AnyClass] = [DateFormatter.self]
      let wrong = try KU.decodeTopLevelObject(of: wrongClasses, forKey: KEY)
      expectUnreachable() // should have error
    }
    catch {
      // expected
    }
    KU.finishDecoding()

    // confirm the that class function works
    do {
      let decoded = try NSKeyedUnarchiver.unarchiveTopLevelObjectWithData(data) as? NSPredicate
      expectEqual(obj, decoded)
    }
    catch {
      expectUnreachableCatch(error)
    }
  }

  FoundationTestSuite.test("NSKeyedUnarchiver/decodeTopLevelObjectOfClass(_:forKey:)/trap") {
    let obj = NSPredicate(value: true)
    let data = createTestArchive()

    var KU = NSKeyedUnarchiver(forReadingWith: data as Data)

    // Even though we're doing non-secure coding, this should still be checked
    // in Swift.
    do {
      var wrongType = try KU.decodeTopLevelObject(of: DateFormatter.self, forKey: KEY)
      expectType((DateFormatter?).self, &wrongType)
    } catch {
      expectUnreachableCatch(error)
    }

    KU.finishDecoding()
  }
}

FoundationTestSuite.test("NotificationCenter/addObserver(_:selector:name:object:)") {
  let obj: AnyObject = "Hello" as NSString
  NotificationCenter.default.addObserver(obj, selector: Selector("blah:"),
                                         name: nil, object: nil)
  let name = "hello"
  NotificationCenter.default.addObserver(obj, selector: Selector("blarg:"),
                                         name: Notification.Name(name),
                                         object: nil)
}

runAllTests()


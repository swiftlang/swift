// RUN: %target-run-simple-swift

import Foundation
import StdlibUnittest

var FoundationTestCase = TestCase("Foundation")

func asNSString(s: String) -> NSString { return s }
func asString(ns: NSString) -> String { return ns }

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

FoundationTestCase.test("NSString") {
  var str = "Hello"
  var nsStr : NSString = str
  assert(nsStr.compare(str).toRaw() == NSComparisonResult.OrderedSame.toRaw())
  assert(nsStr.compare(str) == NSComparisonResult.OrderedSame)
  nsStr = "World"
  str = nsStr
  // FIXME: Shouldn't need coercion here to resolve ambiguity. <rdar://problem/14637688>
  assert(str == asString(nsStr))
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

FoundationTestCase.test("NSNumber") {
  var i = 17
  var d = 3.14159
  var b = true

  // Implicit boxing/explicit unboxing
  var nsNum: NSNumber = i
  expectEqual(i, Int(nsNum))

  nsNum = d
  expectEqual(d, Double(nsNum))

  nsNum = b
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

FoundationTestCase.test("NSArray") {
  // Literals
  var nsArr: NSArray = [ 1, 2.5, "Hello" ]
  assert(nsArr.count == 3)

  // Subscripting
  expectEqual(1, Int(nsArr[0] as NSNumber))
  expectEqual(2.5, Double(nsArr[1] as NSNumber))
  expectTrue((nsArr[2] as NSString).isEqual("Hello"))

  // Iteration
  var result = [String]()
  for x: AnyObject in nsArr {
    result.append((x as NSObject).description)
  }
  expectEqualSequence([ "1", "2.5", "Hello" ], result)
}

FoundationTestCase.test("NSMutableArray") {
  let nsMutableArr: NSMutableArray = ["Constant", "Moon"]
  nsMutableArr[0] = "Inconstant"

  expectEqual(2, nsMutableArr.count)
  expectEqual("Inconstant", nsMutableArr[0] as NSString)
  expectEqual("Moon", nsMutableArr[1] as NSString)
}

FoundationTestCase.test("NSArrayVariadicInit") {
  let variadicArray = NSArray(objects: "A", "B", "C")
  expectEqual(3, variadicArray.count)
}

FoundationTestCase.test("arrayConversions") {
  var nsa = NSArray()
  var aoa: Array<AnyObject> = []

  nsa as Array<AnyObject>
  var nsa2 = NSArray()
  var aoa2: Array<AnyObject> = nsa2

  var nsaoa = aoa as NSArray

  func nsArrayToAnyObjectArray(nsa: NSArray) -> [AnyObject] {
    return nsa
  }

  nsArrayToAnyObjectArray(nsa)
  nsArrayToAnyObjectArray(aoa)
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

FoundationTestCase.test("NSDictionary") {
  var nsDict : NSDictionary = [1 : "Hello", 2 : "World"]
  assert((nsDict[1]! as NSString).isEqual("Hello"))
  assert((nsDict[2]! as NSString).isEqual("World"))

  let nsMutableDict: NSMutableDictionary = ["Hello" : 1, "World" : 2]
  assert(nsMutableDict["Hello"]!.isEqual(1))
  assert(nsMutableDict["World"]!.isEqual(2))
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

FoundationTestCase.test("NSRange") {
  let nsRange = NSRange(1..<5)
  expectEqual("{1, 4}", toString(NSStringFromRange(nsRange)))
}

//===----------------------------------------------------------------------===//
// URLs
//===----------------------------------------------------------------------===//

FoundationTestCase.test("NSURL") {
  let nsURL = NSURL(string: "http://llvm.org")
  expectEqual("http://llvm.org", nsURL.description)
}

//===----------------------------------------------------------------------===//
// Pattern-matching
//===----------------------------------------------------------------------===//

func matchesEither(input: NSNumber, a: NSNumber, b: NSNumber) -> Bool {
  switch input {
  case a, b:
    return true
  default:
    return false
  }
}

FoundationTestCase.test("patternMatching") {
  var one, two, three, oneAgain : NSNumber
  one = NSNumber(int: 1)
  two = NSNumber(int: 2)
  three = NSNumber(int: 3)
  oneAgain = NSNumber(int: 1)
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
    var noteCenter = NSNotificationCenter.defaultCenter()
    noteCenter.removeObserver(self, name: "ReceivedContentNotification", object: nil)
  }
}

FoundationTestCase.test("rdar://17584531") {
  // <rdar://problem/17584531>
  // Type checker used to be confused by this.
  var dict: NSDictionary = [ "status": 200, "people": [ [ "id": 255, "name": [ "first": "John", "last": "Appleseed" ] ] ] ]
  var dict2 = dict["people"]?[0] as NSDictionary
  expectEqual("Optional(255)", toString(dict2["id"]))
}

func staticAssertType<T>(_: T.Type, inout value: T) {}

#if os(OSX)
FoundationTestCase.test("NSRectEdge/constants") {
  // Check that the following constants have the correct type and value.
  //
  // It is valid to hardcode the value in the test.  The way they are currently
  // defined in the SDK makes them ABI for Objective-C code.
  if true {
    var x = NSMinXEdge
    staticAssertType(NSRectEdge.self, &x)
    expectEqual(0, NSMinXEdge)
  }
  if true {
    var x = NSMinYEdge
    staticAssertType(NSRectEdge.self, &x)
    expectEqual(1, NSMinYEdge)
  }
  if true {
    var x = NSMaxXEdge
    staticAssertType(NSRectEdge.self, &x)
    expectEqual(2, NSMaxXEdge)
  }
  if true {
    var x = NSMaxYEdge
    staticAssertType(NSRectEdge.self, &x)
    expectEqual(3, NSMaxYEdge)
  }
}
#endif

runAllTests()


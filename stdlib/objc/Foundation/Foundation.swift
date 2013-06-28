import ObjectiveC
import Foundation // Clang module

func [asmname="swift_StringToNSString"]
convertStringToNSString(string : [byref] String) -> NSString

func [asmname="swift_NSStringToString"]
convertNSStringToString(nsstring : NSString, string : [byref] String)

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

extension String {
  /// \brief Convert from a Swift String to an Objective-C NSString
  func [conversion] __conversion() -> NSString {
    return convertStringToNSString(&this)
  }
}

extension NSString : StringLiteralConvertible {
  /// \brief Convert from an Objective-C NSString to a Swift string.
  func [conversion] __conversion() -> String {
    var result : String
    convertNSStringToString(this, &result)
    return result
  }

  /// \brief Construct an NSString from a string literal.
  // FIXME: Fast-path in compiler/runtime to build a CFConstantString
  typealias StringLiteralType = String
  static func convertFromStringLiteral(s : String) -> NSString {
    return s
  }
}

//===----------------------------------------------------------------------===//
// Numbers
//===----------------------------------------------------------------------===//

// Conversions between NSNumber and various numeric types. The
// conversion to NSNumber is automatic (auto-boxing), while conversion
// back to a specific numeric type requires a cast.
// FIXME: Incomplete list of types.
extension Int {
  constructor (num : NSNumber) {
    value = num.integerValue().value
  }

  func [conversion] __conversion() -> NSNumber {
    // FIXME: Unwanted cast
    return NSNumber.numberWithInteger(this)
  }
}

extension Double {
  constructor (num : NSNumber) {
    value = num.doubleValue().value
  }

  func [conversion] __conversion() -> NSNumber {
    // FIXME: Unwanted cast
    return NSNumber.numberWithDouble(this)
  }
}

extension Bool {
  constructor (num : NSNumber) {
    if num.boolValue() { this = true }
    else { this = false }
  }

  func [conversion] __conversion() -> NSNumber {
    // FIXME: Unwanted cast
    return NSNumber.numberWithBool(this)
  }
}

// Literal support for NSNumber
extension NSNumber : FloatLiteralConvertible, IntegerLiteralConvertible {
  typealias IntegerLiteralType = Int
  static func convertFromIntegerLiteral(i : Int) -> NSNumber {
    return i
  }

  typealias FloatLiteralType = Double
  static func convertFromFloatLiteral(f : Double) -> NSNumber {
    return f
  }
}


//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//
extension NSArray : ArrayLiteralConvertible {
  typealias Element = id

  static func convertFromArrayLiteral(xs:id...) -> NSArray {
    return NSArray.arrayWithObjects(xs.cAddress(), xs.length) as! NSArray
  }
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//
extension NSDictionary : DictionaryLiteralConvertible {
  typealias Key = NSCopyingProto
  typealias Value = id

  static func convertFromDictionaryLiteral(xs:(NSCopyingProto, id)...)
           -> NSDictionary {
    var keys : NSCopyingProto[] = new NSCopyingProto[xs.length]
    var objects : id[] = new id[xs.length]

    for i in 0..xs.length {
      keys[i] = xs[i].0
      objects[i] = xs[i].1
    }

    // FIXME: Unwanted cast
    return NSDictionary.dictionaryWithObjects(objects.cAddress(),
                                              keys.cAddress(),
                                              xs.length) as! NSDictionary
  }
}

//===----------------------------------------------------------------------===//
// Typed Collections
//===----------------------------------------------------------------------===//
struct NSTypedArray<T : NSObject> {
  var array : NSArray

  constructor(array : NSArray) { this.array = array }

  func [conversion] __conversion() -> NSArray { return array }

  subscript (idx : NSUInteger) -> T {
    return array[idx] as! T
  }
}

//===----------------------------------------------------------------------===//
// General objects
//===----------------------------------------------------------------------===//

func [asmname="swift_compareObjects"] _compareObjects(lhs : NSObject, rhs : NSObject) -> CBool

func [asmname="swift_hashObject"] _hashObject(obj : NSObject) -> Int

extension NSObject : Hashable {
  func hashValue() -> Int {
    return _hashObject(this)
  }

  func __equal__(rhs : NSObject) -> Bool {
    return _compareObjects(this, rhs)
  }

  /// \brief Print NSObjects in the REPL
  func replPrint() {
    if this === nil {
      print("<<nil>>")
      return
    }

    var desc : String

    if NSJSONSerialization.isValidJSONObject(this) {
      var jsonData = NSJSONSerialization.dataWithJSONObject(this,
                          options : NSJSONWritingPrettyPrinted,
                          error : UnsafePointer.null())

      if jsonData === nil {
        desc = description() as String
      } else {
        desc = (NSString(initWithData:jsonData,
                            encoding:NSStringEncoding(NSUTF8StringEncoding)) as String)
      }
    } else {
      desc = description() as String
    }

    var first = true
    for line in desc.split('\n') {
      if first {
        first = false
      } else {
        print("\n// ")
      }
      print(line)
    }
  }
}

// A 'nil' that can be used for comparisons, though assignments require a cast.
var nil : NSObject

func === (lhs : NSObject, rhs : NSObject) -> Bool {
  return _compareObjects(lhs, rhs)
}

func !== (lhs : NSObject, rhs : NSObject) -> Bool {
  return !(lhs === rhs)
}

//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//

// NB: This is a class because fast enumeration passes around interior pointers
// to the enumeration state, so the state cannot be moved in memory. We will
// probably need to implement fast enumeration in the compiler as a primitive
// to implement it both correctly and efficiently.
class NSFastEnumerator : Enumerator {
  typealias Element = NSObject

  var enumerable : NSFastEnumerationProto
  var state : NSFastEnumerationState
  var n : NSUInteger
  var count : NSUInteger

  /// Size of ObjectsBuffer, in ids.
  var STACK_BUF_SIZE : NSUInteger { return 4 }

  /// Must have enough space for STACK_BUF_SIZE object references.
  struct ObjectsBuffer {
    var buf : (COpaquePointer, COpaquePointer, COpaquePointer, COpaquePointer)
  }

  var objects : ObjectsBuffer

  func isEmpty() -> Bool {
    return count == 0
  }

  func next() -> NSObject {
    var next = state.itemsPtr[n]
    ++n
    if n == count {
      refresh()
    }
    return next
  }

  func refresh() {
    n = 0
    count = enumerable.countByEnumeratingWithState(
      UnsafePointer.addressOf(&state),
      objects:UnsafePointer(UnsafePointer.addressOf(&objects.buf)),
      count:STACK_BUF_SIZE)
  }

  constructor(enumerable:NSFastEnumerationProto) {
    this.enumerable = enumerable
    this.state.state = 0
    this.refresh()
  }
}

extension NSArray : Enumerable {
  typealias EnumeratorType = NSFastEnumerator

  func getEnumeratorType() -> NSFastEnumerator {
    return NSFastEnumerator(this)
  }
}

extension NSSet : Enumerable {
  typealias EnumeratorType = NSFastEnumerator

  func getEnumeratorType() -> NSFastEnumerator {
    return NSFastEnumerator(this)
  }
}

// FIXME: A class because we can't pass a struct with class fields through an
// [objc] interface without prematurely destroying the references.
class NSDictionaryEnumerator : Enumerator {
  typealias Element = (key:NSObject, value:NSObject)

  var fastEnumerator : NSFastEnumerator
  var dictionary : NSDictionary {
    return fastEnumerator.enumerable as! NSDictionary
  }

  func isEmpty() -> Bool {
    return fastEnumerator.isEmpty()
  }

  func next() -> (key:NSObject, value:NSObject) {
    var key = fastEnumerator.next()
    return (key:key, value:dictionary[key])
  }

  constructor(dict:NSDictionary) {
    this.fastEnumerator = NSFastEnumerator(dict)
  }
}

extension NSDictionary : Enumerable {
  typealias EnumeratorType = NSDictionaryEnumerator

  func getEnumeratorType() -> NSDictionaryEnumerator {
    return NSDictionaryEnumerator(this)
  }
}

//===----------------------------------------------------------------------===//
// Ranges
//===----------------------------------------------------------------------===//

extension IntEnumeratorType {
  func [conversion] __conversion() -> NSRange {
    return NSRange(min, max - min)
  }
}

extension NSRange {
  func [conversion] __conversion() -> IntEnumeratorType {
    var result : IntEnumeratorType
    result.min = location
    result.max = location + length
    result.stride = 1
    return result
  }
}

//===----------------------------------------------------------------------===//
// NSRect
//===----------------------------------------------------------------------===//
extension NSRect {
  constructor(x : CGFloat, y : CGFloat, width : CGFloat, height : CGFloat) {
    origin = CGPoint(x, y)
    size = CGSize(width, height)
  }
}

//===----------------------------------------------------------------------===//
// URLs
//===----------------------------------------------------------------------===//
extension NSURL : StringLiteralConvertible {
  typealias StringLiteralType = String
  static func convertFromStringLiteral(s : String) -> NSURL {
    return NSURL(initWithString:s)
  }
}


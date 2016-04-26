
class NSValue : NSObject, NSCopying, NSSecureCoding {
  func getValue(_ value: UnsafeMutablePointer<Void>)
  var objCType: UnsafePointer<Int8> { get }
  init(bytes value: UnsafePointer<Void>, objCType type: UnsafePointer<Int8>)
}
extension NSValue {
  /*not inherited*/ init(_ value: UnsafePointer<Void>, withObjCType type: UnsafePointer<Int8>)
}
extension NSValue {
  /*not inherited*/ init(nonretainedObject anObject: AnyObject?)
  var nonretainedObjectValue: AnyObject? { get }
  /*not inherited*/ init(pointer pointer: UnsafePointer<Void>?)
  var pointerValue: UnsafeMutablePointer<Void>? { get }
  @discardableResult
  func isEqual(to value: NSValue) -> Bool
}
class NSNumber : NSValue {
  init(value value: Int8)
  init(value value: UInt8)
  init(value value: Int16)
  init(value value: UInt16)
  init(value value: Int32)
  init(value value: UInt32)
  init(value value: Int64)
  init(value value: UInt64)
  init(value value: Float)
  init(value value: Double)
  init(value value: Bool)
  @available(OSX 10.5, *)
  init(value value: Int)
  @available(OSX 10.5, *)
  init(value value: UInt)
  var int8Value: Int8 { get }
  var uint8Value: UInt8 { get }
  var int16Value: Int16 { get }
  var uint16Value: UInt16 { get }
  var int32Value: Int32 { get }
  var uint32Value: UInt32 { get }
  var int64Value: Int64 { get }
  var uint64Value: UInt64 { get }
  var floatValue: Float { get }
  var doubleValue: Double { get }
  var boolValue: Bool { get }
  @available(OSX 10.5, *)
  var intValue: Int { get }
  @available(OSX 10.5, *)
  var uintValue: UInt { get }
  var stringValue: String { get }
  @discardableResult
  func compare(_ otherNumber: NSNumber) -> NSComparisonResult
  @discardableResult
  func isEqual(to number: NSNumber) -> Bool
  @discardableResult
  func description(withLocale locale: AnyObject?) -> String
}

extension NSNumber : FloatLiteralConvertible, IntegerLiteralConvertible, BooleanLiteralConvertible {
}
extension NSNumber {
}

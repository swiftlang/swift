
class NSCoder : NSObject {
  func encodeValue(ofObjCType type: UnsafePointer<Int8>, at addr: UnsafePointer<Void>)
  func encodeDataObject(_ data: NSData)
  func decodeValue(ofObjCType type: UnsafePointer<Int8>, at data: UnsafeMutablePointer<Void>)
  @discardableResult
  func decodeDataObject() -> NSData?
  @discardableResult
  func version(forClassName className: String) -> Int
}

extension NSCoder {
  @warn_unused_result
  func decodeObjectOfClass<DecodedObjectType : NSCoding where DecodedObjectType : NSObject>(_ cls: DecodedObjectType.Type, forKey key: String) -> DecodedObjectType?
  @warn_unused_result
  @nonobjc func decodeObjectOfClasses(_ classes: NSSet?, forKey key: String) -> AnyObject?
  @warn_unused_result
  func decodeTopLevelObject() throws -> AnyObject?
  @warn_unused_result
  func decodeTopLevelObjectForKey(_ key: String) throws -> AnyObject?
  @warn_unused_result
  func decodeTopLevelObjectOfClass<DecodedObjectType : NSCoding where DecodedObjectType : NSObject>(_ cls: DecodedObjectType.Type, forKey key: String) throws -> DecodedObjectType?
  @warn_unused_result
  func decodeTopLevelObjectOfClasses(_ classes: NSSet?, forKey key: String) throws -> AnyObject?
}
extension NSCoder {
  func encode(_ object: AnyObject?)
  func encodeRootObject(_ rootObject: AnyObject)
  func encodeBycopyObject(_ anObject: AnyObject?)
  func encodeByrefObject(_ anObject: AnyObject?)
  func encodeConditionalObject(_ object: AnyObject?)
  func encodeArray(ofObjCType type: UnsafePointer<Int8>, count count: Int, at array: UnsafePointer<Void>)
  func encodeBytes(_ byteaddr: UnsafePointer<Void>?, length length: Int)
  @discardableResult
  func decodeObject() -> AnyObject?
  func decodeArray(ofObjCType itemType: UnsafePointer<Int8>, count count: Int, at array: UnsafeMutablePointer<Void>)
  @discardableResult
  func decodeBytes(withReturnedLength lengthp: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<Void>?
  var systemVersion: UInt32 { get }
  var allowsKeyedCoding: Bool { get }
  func encode(_ objv: AnyObject?, forKey key: String)
  func encodeConditionalObject(_ objv: AnyObject?, forKey key: String)
  func encode(_ boolv: Bool, forKey key: String)
  func encode(_ intv: Int32, forKey key: String)
  func encode(_ intv: Int64, forKey key: String)
  func encode(_ realv: Float, forKey key: String)
  func encode(_ realv: Double, forKey key: String)
  func encodeBytes(_ bytesp: UnsafePointer<UInt8>?, length lenv: Int, forKey key: String)
  @discardableResult
  func containsValue(forKey key: String) -> Bool
  @discardableResult
  func decodeObject(forKey key: String) -> AnyObject?
  @discardableResult
  func decodeBool(forKey key: String) -> Bool
  @discardableResult
  func decodeInt32(forKey key: String) -> Int32
  @discardableResult
  func decodeInt64(forKey key: String) -> Int64
  @discardableResult
  func decodeFloat(forKey key: String) -> Float
  @discardableResult
  func decodeDouble(forKey key: String) -> Double
  @discardableResult
  func decodeBytes(forKey key: String, returnedLength lengthp: UnsafeMutablePointer<Int>?) -> UnsafePointer<UInt8>?
  @available(iOS 2.0, *)
  func encode(_ intv: Int, forKey key: String)
  @available(iOS 2.0, *)
  @discardableResult
  func decodeInteger(forKey key: String) -> Int
  @available(iOS 6.0, *)
  var requiresSecureCoding: Bool { get }
  @available(iOS 6.0, *)
  @discardableResult
  func __decodeObject(ofClasses classes: Set<NSObject>?, forKey key: String) -> AnyObject?
  @available(iOS 6.0, *)
  @discardableResult
  func decodePropertyList(forKey key: String) -> AnyObject?
  @available(iOS 6.0, *)
  var allowedClasses: Set<NSObject>? { get }
  @available(iOS 9.0, *)
  func failWithError(_ error: NSError)
}

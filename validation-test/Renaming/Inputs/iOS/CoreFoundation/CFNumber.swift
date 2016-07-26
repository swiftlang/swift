
class CFBoolean {
}
let kCFBooleanTrue: CFBoolean!
let kCFBooleanFalse: CFBoolean!
@discardableResult
func CFBooleanGetTypeID() -> CFTypeID
@discardableResult
func CFBooleanGetValue(_ boolean: CFBoolean!) -> Bool
enum CFNumberType : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case sInt8Type
  case sInt16Type
  case sInt32Type
  case sInt64Type
  case float32Type
  case float64Type
  case charType
  case shortType
  case intType
  case longType
  case longLongType
  case floatType
  case doubleType
  case cfIndexType
  @available(iOS 2.0, *)
  case nsIntegerType
  @available(iOS 2.0, *)
  case cgFloatType
  static var maxType: CFNumberType { get }
}
class CFNumber {
}
let kCFNumberPositiveInfinity: CFNumber!
let kCFNumberNegativeInfinity: CFNumber!
let kCFNumberNaN: CFNumber!
@discardableResult
func CFNumberGetTypeID() -> CFTypeID
@discardableResult
func CFNumberCreate(_ allocator: CFAllocator!, _ theType: CFNumberType, _ valuePtr: UnsafePointer<Void>!) -> CFNumber!
@discardableResult
func CFNumberGetType(_ number: CFNumber!) -> CFNumberType
@discardableResult
func CFNumberGetByteSize(_ number: CFNumber!) -> CFIndex
@discardableResult
func CFNumberIsFloatType(_ number: CFNumber!) -> Bool
@discardableResult
func CFNumberGetValue(_ number: CFNumber!, _ theType: CFNumberType, _ valuePtr: UnsafeMutablePointer<Void>!) -> Bool
@discardableResult
func CFNumberCompare(_ number: CFNumber!, _ otherNumber: CFNumber!, _ context: UnsafeMutablePointer<Void>!) -> CFComparisonResult

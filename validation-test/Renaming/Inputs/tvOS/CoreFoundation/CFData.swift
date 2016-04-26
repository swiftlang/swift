
class CFData {
}
class CFMutableData {
}
@discardableResult
func CFDataGetTypeID() -> CFTypeID
@discardableResult
func CFDataCreate(_ allocator: CFAllocator!, _ bytes: UnsafePointer<UInt8>!, _ length: CFIndex) -> CFData!
@discardableResult
func CFDataCreateWithBytesNoCopy(_ allocator: CFAllocator!, _ bytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ bytesDeallocator: CFAllocator!) -> CFData!
@discardableResult
func CFDataCreateCopy(_ allocator: CFAllocator!, _ theData: CFData!) -> CFData!
@discardableResult
func CFDataCreateMutable(_ allocator: CFAllocator!, _ capacity: CFIndex) -> CFMutableData!
@discardableResult
func CFDataCreateMutableCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ theData: CFData!) -> CFMutableData!
@discardableResult
func CFDataGetLength(_ theData: CFData!) -> CFIndex
@discardableResult
func CFDataGetBytePtr(_ theData: CFData!) -> UnsafePointer<UInt8>!
@discardableResult
func CFDataGetMutableBytePtr(_ theData: CFMutableData!) -> UnsafeMutablePointer<UInt8>!
func CFDataGetBytes(_ theData: CFData!, _ range: CFRange, _ buffer: UnsafeMutablePointer<UInt8>!)
func CFDataSetLength(_ theData: CFMutableData!, _ length: CFIndex)
func CFDataIncreaseLength(_ theData: CFMutableData!, _ extraLength: CFIndex)
func CFDataAppendBytes(_ theData: CFMutableData!, _ bytes: UnsafePointer<UInt8>!, _ length: CFIndex)
func CFDataReplaceBytes(_ theData: CFMutableData!, _ range: CFRange, _ newBytes: UnsafePointer<UInt8>!, _ newLength: CFIndex)
func CFDataDeleteBytes(_ theData: CFMutableData!, _ range: CFRange)
@available(tvOS 4.0, *)
struct CFDataSearchFlags : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var backwards: CFDataSearchFlags { get }
  static var anchored: CFDataSearchFlags { get }
}
@available(tvOS 4.0, *)
@discardableResult
func CFDataFind(_ theData: CFData!, _ dataToFind: CFData!, _ searchRange: CFRange, _ compareOptions: CFDataSearchFlags) -> CFRange

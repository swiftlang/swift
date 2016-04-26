
class CTRunDelegate {
}
@available(watchOS 2.0, *)
@discardableResult
func CTRunDelegateGetTypeID() -> CFTypeID
typealias CTRunDelegateDeallocateCallback = @convention(c) (UnsafeMutablePointer<Void>) -> Void
typealias CTRunDelegateGetAscentCallback = @convention(c) (UnsafeMutablePointer<Void>) -> CGFloat
typealias CTRunDelegateGetDescentCallback = @convention(c) (UnsafeMutablePointer<Void>) -> CGFloat
typealias CTRunDelegateGetWidthCallback = @convention(c) (UnsafeMutablePointer<Void>) -> CGFloat
struct CTRunDelegateCallbacks {
  var version: CFIndex
  var dealloc: CTRunDelegateDeallocateCallback
  var getAscent: CTRunDelegateGetAscentCallback
  var getDescent: CTRunDelegateGetDescentCallback
  var getWidth: CTRunDelegateGetWidthCallback
}
var kCTRunDelegateVersion1: Int { get }
var kCTRunDelegateCurrentVersion: Int { get }
@available(watchOS 2.0, *)
@discardableResult
func CTRunDelegateCreate(_ callbacks: UnsafePointer<CTRunDelegateCallbacks>, _ refCon: UnsafeMutablePointer<Void>?) -> CTRunDelegate?
@available(watchOS 2.0, *)
@discardableResult
func CTRunDelegateGetRefCon(_ runDelegate: CTRunDelegate) -> UnsafeMutablePointer<Void>

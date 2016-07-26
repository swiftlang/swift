
enum CFStreamStatus : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case notOpen
  case opening
  case open
  case reading
  case writing
  case atEnd
  case closed
  case error
}
struct CFStreamEventType : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var openCompleted: CFStreamEventType { get }
  static var hasBytesAvailable: CFStreamEventType { get }
  static var canAcceptBytes: CFStreamEventType { get }
  static var errorOccurred: CFStreamEventType { get }
  static var endEncountered: CFStreamEventType { get }
}
struct CFStreamClientContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!
  var release: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafeMutablePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!)!, release release: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafeMutablePointer<Void>!) -> Unmanaged<CFString>!)!)
}
class CFReadStream {
}
class CFWriteStream {
}
typealias CFReadStreamClientCallBack = @convention(c) (CFReadStream!, CFStreamEventType, UnsafeMutablePointer<Void>!) -> Void
typealias CFWriteStreamClientCallBack = @convention(c) (CFWriteStream!, CFStreamEventType, UnsafeMutablePointer<Void>!) -> Void
@discardableResult
func CFReadStreamGetTypeID() -> CFTypeID
@discardableResult
func CFWriteStreamGetTypeID() -> CFTypeID
let kCFStreamPropertyDataWritten: CFString!
@discardableResult
func CFReadStreamCreateWithBytesNoCopy(_ alloc: CFAllocator!, _ bytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ bytesDeallocator: CFAllocator!) -> CFReadStream!
@discardableResult
func CFWriteStreamCreateWithBuffer(_ alloc: CFAllocator!, _ buffer: UnsafeMutablePointer<UInt8>!, _ bufferCapacity: CFIndex) -> CFWriteStream!
@discardableResult
func CFWriteStreamCreateWithAllocatedBuffers(_ alloc: CFAllocator!, _ bufferAllocator: CFAllocator!) -> CFWriteStream!
@discardableResult
func CFReadStreamCreateWithFile(_ alloc: CFAllocator!, _ fileURL: CFURL!) -> CFReadStream!
@discardableResult
func CFWriteStreamCreateWithFile(_ alloc: CFAllocator!, _ fileURL: CFURL!) -> CFWriteStream!
func CFStreamCreateBoundPair(_ alloc: CFAllocator!, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>!, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>!, _ transferBufferSize: CFIndex)
let kCFStreamPropertyAppendToFile: CFString!
let kCFStreamPropertyFileCurrentOffset: CFString!
let kCFStreamPropertySocketNativeHandle: CFString!
let kCFStreamPropertySocketRemoteHostName: CFString!
let kCFStreamPropertySocketRemotePortNumber: CFString!
func CFStreamCreatePairWithSocket(_ alloc: CFAllocator!, _ sock: CFSocketNativeHandle, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>!, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>!)
func CFStreamCreatePairWithSocketToHost(_ alloc: CFAllocator!, _ host: CFString!, _ port: UInt32, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>!, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>!)
func CFStreamCreatePairWithPeerSocketSignature(_ alloc: CFAllocator!, _ signature: UnsafePointer<CFSocketSignature>!, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>!, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>!)
@discardableResult
func CFReadStreamGetStatus(_ stream: CFReadStream!) -> CFStreamStatus
@discardableResult
func CFWriteStreamGetStatus(_ stream: CFWriteStream!) -> CFStreamStatus
@available(OSX 10.5, *)
@discardableResult
func CFReadStreamCopyError(_ stream: CFReadStream!) -> CFError!
@available(OSX 10.5, *)
@discardableResult
func CFWriteStreamCopyError(_ stream: CFWriteStream!) -> CFError!
@discardableResult
func CFReadStreamOpen(_ stream: CFReadStream!) -> Bool
@discardableResult
func CFWriteStreamOpen(_ stream: CFWriteStream!) -> Bool
func CFReadStreamClose(_ stream: CFReadStream!)
func CFWriteStreamClose(_ stream: CFWriteStream!)
@discardableResult
func CFReadStreamHasBytesAvailable(_ stream: CFReadStream!) -> Bool
@discardableResult
func CFReadStreamRead(_ stream: CFReadStream!, _ buffer: UnsafeMutablePointer<UInt8>!, _ bufferLength: CFIndex) -> CFIndex
@discardableResult
func CFReadStreamGetBuffer(_ stream: CFReadStream!, _ maxBytesToRead: CFIndex, _ numBytesRead: UnsafeMutablePointer<CFIndex>!) -> UnsafePointer<UInt8>!
@discardableResult
func CFWriteStreamCanAcceptBytes(_ stream: CFWriteStream!) -> Bool
@discardableResult
func CFWriteStreamWrite(_ stream: CFWriteStream!, _ buffer: UnsafePointer<UInt8>!, _ bufferLength: CFIndex) -> CFIndex
@discardableResult
func CFReadStreamCopyProperty(_ stream: CFReadStream!, _ propertyName: CFString!) -> CFTypeRef!
@discardableResult
func CFWriteStreamCopyProperty(_ stream: CFWriteStream!, _ propertyName: CFString!) -> CFTypeRef!
@discardableResult
func CFReadStreamSetProperty(_ stream: CFReadStream!, _ propertyName: CFString!, _ propertyValue: CFTypeRef!) -> Bool
@discardableResult
func CFWriteStreamSetProperty(_ stream: CFWriteStream!, _ propertyName: CFString!, _ propertyValue: CFTypeRef!) -> Bool
@discardableResult
func CFReadStreamSetClient(_ stream: CFReadStream!, _ streamEvents: CFOptionFlags, _ clientCB: CFReadStreamClientCallBack!, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>!) -> Bool
@discardableResult
func CFWriteStreamSetClient(_ stream: CFWriteStream!, _ streamEvents: CFOptionFlags, _ clientCB: CFWriteStreamClientCallBack!, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>!) -> Bool
func CFReadStreamScheduleWithRunLoop(_ stream: CFReadStream!, _ runLoop: CFRunLoop!, _ runLoopMode: CFString!)
func CFWriteStreamScheduleWithRunLoop(_ stream: CFWriteStream!, _ runLoop: CFRunLoop!, _ runLoopMode: CFString!)
func CFReadStreamUnscheduleFromRunLoop(_ stream: CFReadStream!, _ runLoop: CFRunLoop!, _ runLoopMode: CFString!)
func CFWriteStreamUnscheduleFromRunLoop(_ stream: CFWriteStream!, _ runLoop: CFRunLoop!, _ runLoopMode: CFString!)
@available(OSX 10.9, *)
func CFReadStreamSetDispatchQueue(_ stream: CFReadStream!, _ q: dispatch_queue_t!)
@available(OSX 10.9, *)
func CFWriteStreamSetDispatchQueue(_ stream: CFWriteStream!, _ q: dispatch_queue_t!)
@available(OSX 10.9, *)
@discardableResult
func CFReadStreamCopyDispatchQueue(_ stream: CFReadStream!) -> dispatch_queue_t!
@available(OSX 10.9, *)
@discardableResult
func CFWriteStreamCopyDispatchQueue(_ stream: CFWriteStream!) -> dispatch_queue_t!
enum CFStreamErrorDomain : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case custom
  case POSIX
  case macOSStatus
}
struct CFStreamError {
  var domain: CFIndex
  var error: Int32
  init()
  init(domain domain: CFIndex, error error: Int32)
}
@discardableResult
func CFReadStreamGetError(_ stream: CFReadStream!) -> CFStreamError
@discardableResult
func CFWriteStreamGetError(_ stream: CFWriteStream!) -> CFStreamError

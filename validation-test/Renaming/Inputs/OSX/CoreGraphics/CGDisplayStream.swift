
class CGDisplayStream {
}
class CGDisplayStreamUpdate {
}
enum CGDisplayStreamUpdateRectType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case refreshedRects
  case movedRects
  case dirtyRects
  case reducedDirtyRects
}
enum CGDisplayStreamFrameStatus : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case frameComplete
  case frameIdle
  case frameBlank
  case stopped
}
typealias CGDisplayStreamFrameAvailableHandler = (CGDisplayStreamFrameStatus, UInt64, IOSurface?, CGDisplayStreamUpdate?) -> Void
extension CGDisplayStreamUpdate {
  @available(OSX 10.8, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.8, *)
  @discardableResult
  func getRects(_ rectType: CGDisplayStreamUpdateRectType, rectCount rectCount: UnsafeMutablePointer<Int>) -> UnsafePointer<CGRect>?
  @available(OSX 10.8, *)
  init?(mergedUpdateFirstUpdate firstUpdate: CGDisplayStreamUpdate?, secondUpdate secondUpdate: CGDisplayStreamUpdate?)
  @available(OSX 10.8, *)
  func getMovedRectsDelta(dx dx: UnsafeMutablePointer<CGFloat>, dy dy: UnsafeMutablePointer<CGFloat>)
  @available(OSX 10.8, *)
  var dropCount: Int { get }
}
extension CGDisplayStream {
  @available(OSX 10.8, *)
  class let sourceRect: CFString
  @available(OSX 10.8, *)
  class let destinationRect: CFString
  @available(OSX 10.8, *)
  class let preserveAspectRatio: CFString
  @available(OSX 10.8, *)
  class let colorSpace: CFString
  @available(OSX 10.8, *)
  class let minimumFrameTime: CFString
  @available(OSX 10.8, *)
  class let showCursor: CFString
  @available(OSX 10.8, *)
  class let queueDepth: CFString
  @available(OSX 10.8, *)
  class let yCbCrMatrix: CFString
  @available(OSX 10.8, *)
  class let yCbCrMatrix_ITU_R_709_2: CFString
  @available(OSX 10.8, *)
  class let yCbCrMatrix_ITU_R_601_4: CFString
  @available(OSX 10.8, *)
  class let yCbCrMatrix_SMPTE_240M_1995: CFString
  @available(OSX 10.8, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.8, *)
  init?(display display: CGDirectDisplayID, outputWidth outputWidth: Int, outputHeight outputHeight: Int, pixelFormat pixelFormat: Int32, properties properties: CFDictionary?, handler handler: CGDisplayStreamFrameAvailableHandler?)
  @available(OSX 10.8, *)
  init?(withDispatchQueueDisplay display: CGDirectDisplayID, outputWidth outputWidth: Int, outputHeight outputHeight: Int, pixelFormat pixelFormat: Int32, properties properties: CFDictionary?, queue queue: dispatch_queue_t, handler handler: CGDisplayStreamFrameAvailableHandler?)
  @available(OSX 10.8, *)
  @discardableResult
  func start() -> CGError
  @available(OSX 10.8, *)
  @discardableResult
  func stop() -> CGError
  @available(OSX 10.8, *)
  var runLoopSource: CFRunLoopSource? { get }
}


var NSImageRepMatchesDevice: Int { get }
class NSImageRep : NSObject, NSCopying, NSCoding {
  @discardableResult
  func draw() -> Bool
  @discardableResult
  func draw(at point: NSPoint) -> Bool
  @discardableResult
  func draw(in rect: NSRect) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  func draw(in dstSpacePortionRect: NSRect, from srcSpacePortionRect: NSRect, operation op: NSCompositingOperation, fraction requestedAlpha: CGFloat, respectFlipped respectContextIsFlipped: Bool, hints hints: [String : AnyObject]?) -> Bool
  var size: NSSize
  var hasAlpha: Bool
  var isOpaque: Bool
  var colorSpaceName: String
  var bitsPerSample: Int
  var pixelsWide: Int
  var pixelsHigh: Int
  class func registerClass(_ imageRepClass: AnyClass)
  class func unregisterImageRepClass(_ imageRepClass: AnyClass)
  @discardableResult
  class func registeredImageRepClasses() -> [AnyClass]
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageRepClassForType: instead")
  @discardableResult
  class func imageRepClass(forFileType type: String) -> AnyClass?
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageRepClassForType: instead")
  @discardableResult
  class func imageRepClass(forPasteboardType type: String) -> AnyClass?
  @available(OSX 10.5, *)
  @discardableResult
  class func imageRepClass(forType type: String) -> AnyClass?
  @discardableResult
  class func imageRepClass(for data: NSData) -> AnyClass?
  @discardableResult
  class func canInit(with data: NSData) -> Bool
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageUnfilteredTypes instead")
  @discardableResult
  class func imageUnfilteredFileTypes() -> [String]
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageUnfilteredTypes instead")
  @discardableResult
  class func imageUnfilteredPasteboardTypes() -> [String]
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageTypes instead")
  @discardableResult
  class func imageFileTypes() -> [String]
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use +imageTypes instead")
  @discardableResult
  class func imagePasteboardTypes() -> [String]
  @available(OSX 10.5, *)
  @discardableResult
  class func imageUnfilteredTypes() -> [String]
  @available(OSX 10.5, *)
  @discardableResult
  class func imageTypes() -> [String]
  @discardableResult
  class func canInit(with pasteboard: NSPasteboard) -> Bool
  @discardableResult
  class func imageReps(withContentsOfFile filename: String) -> [NSImageRep]?
  /*not inherited*/ init?(contentsOfFile filename: String)
  @discardableResult
  class func imageReps(withContentsOf url: NSURL) -> [NSImageRep]?
  /*not inherited*/ init?(contentsOf url: NSURL)
  @discardableResult
  class func imageReps(with pasteboard: NSPasteboard) -> [NSImageRep]?
  /*not inherited*/ init?(pasteboard pasteboard: NSPasteboard)
  @available(OSX 10.6, *)
  @discardableResult
  func cgImage(forProposedRect proposedDestRect: UnsafeMutablePointer<NSRect>?, context context: NSGraphicsContext?, hints hints: [String : AnyObject]?) -> CGImage?
}
struct __repFlags {
  var hasAlpha: UInt32
  var isOpaque: UInt32
  @available(*, deprecated)
  var cacheParamsComputed: UInt32
  @available(*, deprecated)
  var cacheAlphaComputed: UInt32
  var loadState: UInt32
  @available(*, deprecated)
  var keepCacheWindow: UInt32
  var reserved: UInt32
  var bitsPerSample: UInt32
  var gsaved: UInt32
  init()
  init(hasAlpha hasAlpha: UInt32, isOpaque isOpaque: UInt32, cacheParamsComputed cacheParamsComputed: UInt32, cacheAlphaComputed cacheAlphaComputed: UInt32, loadState loadState: UInt32, keepCacheWindow keepCacheWindow: UInt32, reserved reserved: UInt32, bitsPerSample bitsPerSample: UInt32, gsaved gsaved: UInt32)
}
let NSImageRepRegistryDidChangeNotification: String

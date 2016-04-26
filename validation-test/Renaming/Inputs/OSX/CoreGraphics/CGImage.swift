
class CGImage {
}
enum CGImageAlphaInfo : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case none
  case premultipliedLast
  case premultipliedFirst
  case last
  case first
  case noneSkipLast
  case noneSkipFirst
  case alphaOnly
}
@available(OSX 10.4, *)
struct CGBitmapInfo : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var alphaInfoMask: CGBitmapInfo { get }
  static var floatComponents: CGBitmapInfo { get }
  static var byteOrderMask: CGBitmapInfo { get }
  static var byteOrder16Little: CGBitmapInfo { get }
  static var byteOrder32Little: CGBitmapInfo { get }
  static var byteOrder16Big: CGBitmapInfo { get }
  static var byteOrder32Big: CGBitmapInfo { get }
}
extension CGImage {
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.0, *)
  init?(width width: Int, height height: Int, bitsPerComponent bitsPerComponent: Int, bitsPerPixel bitsPerPixel: Int, bytesPerRow bytesPerRow: Int, space space: CGColorSpace?, bitmapInfo bitmapInfo: CGBitmapInfo, provider provider: CGDataProvider?, decode decode: UnsafePointer<CGFloat>?, shouldInterpolate shouldInterpolate: Bool, intent intent: CGColorRenderingIntent)
  @available(OSX 10.0, *)
  init?(maskWidth width: Int, height height: Int, bitsPerComponent bitsPerComponent: Int, bitsPerPixel bitsPerPixel: Int, bytesPerRow bytesPerRow: Int, provider provider: CGDataProvider?, decode decode: UnsafePointer<CGFloat>?, shouldInterpolate shouldInterpolate: Bool)
  @available(OSX 10.4, *)
  init?(copy image: CGImage?)
  @available(OSX 10.1, *)
  init?(withJPEGDataProviderSource source: CGDataProvider?, decode decode: UnsafePointer<CGFloat>?, shouldInterpolate shouldInterpolate: Bool, intent intent: CGColorRenderingIntent)
  @available(OSX 10.2, *)
  init?(withPNGDataProviderSource source: CGDataProvider?, decode decode: UnsafePointer<CGFloat>?, shouldInterpolate shouldInterpolate: Bool, intent intent: CGColorRenderingIntent)
  @available(OSX 10.4, *)
  init?(withImageInRectImage image: CGImage?, rect rect: CGRect)
  @available(OSX 10.4, *)
  init?(withMaskImage image: CGImage?, mask mask: CGImage?)
  @available(OSX 10.4, *)
  init?(withMaskingColorsImage image: CGImage?, components components: UnsafePointer<CGFloat>?)
  @available(OSX 10.3, *)
  init?(copyWithColorSpaceImage image: CGImage?, space space: CGColorSpace?)
  @available(OSX 10.0, *)
  @discardableResult
  func isMask() -> Bool
  @available(OSX 10.0, *)
  var width: Int { get }
  @available(OSX 10.0, *)
  var height: Int { get }
  @available(OSX 10.0, *)
  var bitsPerComponent: Int { get }
  @available(OSX 10.0, *)
  var bitsPerPixel: Int { get }
  @available(OSX 10.0, *)
  var bytesPerRow: Int { get }
  @available(OSX 10.0, *)
  var colorSpace: CGColorSpace? { get }
  @available(OSX 10.0, *)
  var alphaInfo: CGImageAlphaInfo { get }
  @available(OSX 10.0, *)
  var dataProvider: CGDataProvider? { get }
  @available(OSX 10.0, *)
  var decode: UnsafePointer<CGFloat>? { get }
  @available(OSX 10.0, *)
  var shouldInterpolate: Bool { get }
  @available(OSX 10.0, *)
  var renderingIntent: CGColorRenderingIntent { get }
  @available(OSX 10.4, *)
  var bitmapInfo: CGBitmapInfo { get }
  @available(OSX 10.11, *)
  var utType: CFString? { get }
}

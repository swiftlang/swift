
typealias CGBitmapContextReleaseDataCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?) -> Void
extension CGContext {
  @available(OSX 10.6, *)
  init?(bitmapWithData data: UnsafeMutablePointer<Void>?, width width: Int, height height: Int, bitsPerComponent bitsPerComponent: Int, bytesPerRow bytesPerRow: Int, space space: CGColorSpace?, bitmapInfo bitmapInfo: UInt32, releaseCallback releaseCallback: CGBitmapContextReleaseDataCallback?, releaseInfo releaseInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.0, *)
  init?(bitmapData data: UnsafeMutablePointer<Void>?, width width: Int, height height: Int, bitsPerComponent bitsPerComponent: Int, bytesPerRow bytesPerRow: Int, space space: CGColorSpace?, bitmapInfo bitmapInfo: UInt32)
  @available(OSX 10.2, *)
  var bitmapData: UnsafeMutablePointer<Void>? { get }
  @available(OSX 10.2, *)
  var bitmapWidth: Int { get }
  @available(OSX 10.2, *)
  var bitmapHeight: Int { get }
  @available(OSX 10.2, *)
  var bitmapBitsPerComponent: Int { get }
  @available(OSX 10.2, *)
  var bitmapBitsPerPixel: Int { get }
  @available(OSX 10.2, *)
  var bitmapBytesPerRow: Int { get }
  @available(OSX 10.2, *)
  var bitmapColorSpace: CGColorSpace? { get }
  @available(OSX 10.2, *)
  var bitmapAlphaInfo: CGImageAlphaInfo { get }
  @available(OSX 10.4, *)
  var bitmapInfo: CGBitmapInfo { get }
  @available(OSX 10.4, *)
  @discardableResult
  func makeImageFromBitmap() -> CGImage?
}


protocol CAMetalDrawable : MTLDrawable {
  @available(OSX 10.11, *)
  var texture: MTLTexture { get }
  @available(OSX 10.11, *)
  var layer: CAMetalLayer { get }
}
@available(OSX 10.11, *)
class CAMetalLayer : CALayer {
  var device: MTLDevice?
  var pixelFormat: MTLPixelFormat
  var framebufferOnly: Bool
  var drawableSize: CGSize
  @discardableResult
  func nextDrawable() -> CAMetalDrawable?
  var presentsWithTransaction: Bool
  var colorspace: CGColorSpace
  var wantsExtendedDynamicRangeContent: Bool
}


protocol CAMetalDrawable : MTLDrawable {
  @available(tvOS 8.0, *)
  var texture: MTLTexture { get }
  @available(tvOS 8.0, *)
  var layer: CAMetalLayer { get }
}
@available(tvOS 8.0, *)
class CAMetalLayer : CALayer {
  var device: MTLDevice?
  var pixelFormat: MTLPixelFormat
  var framebufferOnly: Bool
  var drawableSize: CGSize
  @discardableResult
  func nextDrawable() -> CAMetalDrawable?
  var presentsWithTransaction: Bool
}

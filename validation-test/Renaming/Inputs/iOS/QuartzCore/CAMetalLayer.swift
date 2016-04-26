
protocol CAMetalDrawable : MTLDrawable {
  @available(iOS 8.0, *)
  var texture: MTLTexture { get }
  @available(iOS 8.0, *)
  var layer: CAMetalLayer { get }
}
@available(iOS 8.0, *)
class CAMetalLayer : CALayer {
  var device: MTLDevice?
  var pixelFormat: MTLPixelFormat
  var framebufferOnly: Bool
  var drawableSize: CGSize
  @discardableResult
  func nextDrawable() -> CAMetalDrawable?
  var presentsWithTransaction: Bool
}

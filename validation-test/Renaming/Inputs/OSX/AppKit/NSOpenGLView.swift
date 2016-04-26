
class NSOpenGLView : NSView {
  @discardableResult
  class func defaultPixelFormat() -> NSOpenGLPixelFormat
  init?(frame frameRect: NSRect, pixelFormat format: NSOpenGLPixelFormat?)
  var openGLContext: NSOpenGLContext?
  func clearGLContext()
  func update()
  func reshape()
  var pixelFormat: NSOpenGLPixelFormat?
  func prepareOpenGL()
}
extension NSView {
  @available(OSX 10.7, *)
  var wantsBestResolutionOpenGLSurface: Bool
}
extension NSView {
  @available(OSX 10.11, *)
  var wantsExtendedDynamicRangeOpenGLSurface: Bool
}


let kEAGLDrawablePropertyRetainedBacking: String
let kEAGLDrawablePropertyColorFormat: String
let kEAGLColorFormatRGBA8: String
let kEAGLColorFormatRGB565: String
@available(iOS 7.0, *)
let kEAGLColorFormatSRGBA8: String
protocol EAGLDrawable {
  @available(iOS 2.0, *)
  var drawableProperties: [NSObject : AnyObject]! { get set }
}
extension EAGLContext {
  @discardableResult
  func renderbufferStorage(_ target: Int, from drawable: EAGLDrawable!) -> Bool
  @discardableResult
  func presentRenderbuffer(_ target: Int) -> Bool
}

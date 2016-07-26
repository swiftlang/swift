
@available(iOS 5.0, *)
enum GLKViewDrawableColorFormat : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case RGBA8888
  case RGB565
  case SRGBA8888
}
@available(iOS 5.0, *)
enum GLKViewDrawableDepthFormat : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case formatNone
  case format16
  case format24
}
@available(iOS 5.0, *)
enum GLKViewDrawableStencilFormat : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case formatNone
  case format8
}
@available(iOS 5.0, *)
enum GLKViewDrawableMultisample : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case multisampleNone
  case multisample4X
}
@available(iOS 5.0, *)
class GLKView : UIView, NSCoding {
  init(frame frame: CGRect, context context: EAGLContext)
  @IBOutlet unowned(unsafe) var delegate: @sil_unmanaged GLKViewDelegate?
  var context: EAGLContext
  var drawableWidth: Int { get }
  var drawableHeight: Int { get }
  var drawableColorFormat: GLKViewDrawableColorFormat
  var drawableDepthFormat: GLKViewDrawableDepthFormat
  var drawableStencilFormat: GLKViewDrawableStencilFormat
  var drawableMultisample: GLKViewDrawableMultisample
  func bindDrawable()
  func deleteDrawable()
  var snapshot: UIImage { get }
  var enableSetNeedsDisplay: Bool
  func display()
}
protocol GLKViewDelegate : NSObjectProtocol {
  @available(iOS 5.0, *)
  func glkView(_ view: GLKView, drawIn rect: CGRect)
}

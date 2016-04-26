
@available(tvOS 3.2, *)
let UIScreenDidConnectNotification: String
@available(tvOS 3.2, *)
let UIScreenDidDisconnectNotification: String
@available(tvOS 3.2, *)
let UIScreenModeDidChangeNotification: String
@available(tvOS 5.0, *)
let UIScreenBrightnessDidChangeNotification: String
enum UIScreenOverscanCompensation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case scale
  case insetBounds
  @available(tvOS 9.0, *)
  case none
  @available(tvOS, introduced: 5.0, deprecated: 9.0, message: "Use UIScreenOverscanCompensationNone")
  static var insetApplicationFrame: UIScreenOverscanCompensation { get }
}
@available(tvOS 2.0, *)
class UIScreen : NSObject, UITraitEnvironment {
  @available(tvOS 3.2, *)
  @discardableResult
  class func screens() -> [UIScreen]
  @discardableResult
  class func main() -> UIScreen
  var bounds: CGRect { get }
  @available(tvOS 4.0, *)
  var scale: CGFloat { get }
  @available(tvOS 3.2, *)
  var currentMode: UIScreenMode? { get }
  @available(tvOS 5.0, *)
  var overscanCompensation: UIScreenOverscanCompensation
  @available(tvOS 9.0, *)
  var overscanCompensationInsets: UIEdgeInsets { get }
  @available(tvOS 4.3, *)
  var mirrored: UIScreen? { get }
  @available(tvOS 8.0, *)
  var coordinateSpace: UICoordinateSpace { get }
  @available(tvOS 8.0, *)
  var fixedCoordinateSpace: UICoordinateSpace { get }
  @available(tvOS 8.0, *)
  var nativeBounds: CGRect { get }
  @available(tvOS 8.0, *)
  var nativeScale: CGFloat { get }
  @available(tvOS 4.0, *)
  @discardableResult
  func displayLink(withTarget target: AnyObject, selector sel: Selector) -> CADisplayLink?
  @available(tvOS 9.0, *)
  weak var focusedView: @sil_weak UIView? { get }
  @available(tvOS 9.0, *)
  var supportsFocus: Bool { get }
}
extension UIScreen {
  @available(tvOS 7.0, *)
  @discardableResult
  func snapshotView(afterScreenUpdates afterUpdates: Bool) -> UIView
}

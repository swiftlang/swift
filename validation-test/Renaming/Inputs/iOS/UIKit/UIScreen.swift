
@available(iOS 3.2, *)
let UIScreenDidConnectNotification: String
@available(iOS 3.2, *)
let UIScreenDidDisconnectNotification: String
@available(iOS 3.2, *)
let UIScreenModeDidChangeNotification: String
@available(iOS 5.0, *)
let UIScreenBrightnessDidChangeNotification: String
enum UIScreenOverscanCompensation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case scale
  case insetBounds
  @available(iOS 9.0, *)
  case none
  @available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use UIScreenOverscanCompensationNone")
  static var insetApplicationFrame: UIScreenOverscanCompensation { get }
}
@available(iOS 2.0, *)
class UIScreen : NSObject, UITraitEnvironment {
  @available(iOS 3.2, *)
  @discardableResult
  class func screens() -> [UIScreen]
  @discardableResult
  class func main() -> UIScreen
  var bounds: CGRect { get }
  @available(iOS 4.0, *)
  var scale: CGFloat { get }
  @available(iOS 3.2, *)
  var availableModes: [UIScreenMode] { get }
  @available(iOS 4.3, *)
  var preferredMode: UIScreenMode? { get }
  @available(iOS 3.2, *)
  var currentMode: UIScreenMode?
  @available(iOS 5.0, *)
  var overscanCompensation: UIScreenOverscanCompensation
  @available(iOS 9.0, *)
  var overscanCompensationInsets: UIEdgeInsets { get }
  @available(iOS 4.3, *)
  var mirrored: UIScreen? { get }
  @available(iOS 5.0, *)
  var brightness: CGFloat
  @available(iOS 5.0, *)
  var wantsSoftwareDimming: Bool
  @available(iOS 8.0, *)
  var coordinateSpace: UICoordinateSpace { get }
  @available(iOS 8.0, *)
  var fixedCoordinateSpace: UICoordinateSpace { get }
  @available(iOS 8.0, *)
  var nativeBounds: CGRect { get }
  @available(iOS 8.0, *)
  var nativeScale: CGFloat { get }
  @available(iOS 4.0, *)
  @discardableResult
  func displayLink(withTarget target: AnyObject, selector sel: Selector) -> CADisplayLink?
  @available(iOS 9.0, *)
  weak var focusedView: @sil_weak UIView? { get }
  @available(iOS 9.0, *)
  var supportsFocus: Bool { get }
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use -[UIScreen bounds]")
  var applicationFrame: CGRect { get }
}
extension UIScreen {
  @available(iOS 7.0, *)
  @discardableResult
  func snapshotView(afterScreenUpdates afterUpdates: Bool) -> UIView
}

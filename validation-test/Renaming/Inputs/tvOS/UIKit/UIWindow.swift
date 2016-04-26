
typealias UIWindowLevel = CGFloat
@available(tvOS 2.0, *)
class UIWindow : UIView {
  @available(tvOS 3.2, *)
  var screen: UIScreen
  var windowLevel: UIWindowLevel
  var isKeyWindow: Bool { get }
  func becomeKeyWindow()
  func resignKeyWindow()
  func makeKeyWindow()
  func makeKeyAndVisible()
  @available(tvOS 4.0, *)
  var rootViewController: UIViewController?
  func send(_ event: UIEvent)
  @discardableResult
  func convert(_ point: CGPoint, to window: UIWindow?) -> CGPoint
  @discardableResult
  func convert(_ point: CGPoint, from window: UIWindow?) -> CGPoint
  @discardableResult
  func convert(_ rect: CGRect, to window: UIWindow?) -> CGRect
  @discardableResult
  func convert(_ rect: CGRect, from window: UIWindow?) -> CGRect
}
let UIWindowLevelNormal: UIWindowLevel
let UIWindowLevelAlert: UIWindowLevel
let UIWindowDidBecomeVisibleNotification: String
let UIWindowDidBecomeHiddenNotification: String
let UIWindowDidBecomeKeyNotification: String
let UIWindowDidResignKeyNotification: String

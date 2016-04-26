
typealias UIWindowLevel = CGFloat
@available(iOS 2.0, *)
class UIWindow : UIView {
  @available(iOS 3.2, *)
  var screen: UIScreen
  var windowLevel: UIWindowLevel
  var isKeyWindow: Bool { get }
  func becomeKeyWindow()
  func resignKeyWindow()
  func makeKeyWindow()
  func makeKeyAndVisible()
  @available(iOS 4.0, *)
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
let UIWindowLevelStatusBar: UIWindowLevel
let UIWindowDidBecomeVisibleNotification: String
let UIWindowDidBecomeHiddenNotification: String
let UIWindowDidBecomeKeyNotification: String
let UIWindowDidResignKeyNotification: String
let UIKeyboardWillShowNotification: String
let UIKeyboardDidShowNotification: String
let UIKeyboardWillHideNotification: String
let UIKeyboardDidHideNotification: String
@available(iOS 3.2, *)
let UIKeyboardFrameBeginUserInfoKey: String
@available(iOS 3.2, *)
let UIKeyboardFrameEndUserInfoKey: String
@available(iOS 3.0, *)
let UIKeyboardAnimationDurationUserInfoKey: String
@available(iOS 3.0, *)
let UIKeyboardAnimationCurveUserInfoKey: String
@available(iOS 9.0, *)
let UIKeyboardIsLocalUserInfoKey: String
@available(iOS 5.0, *)
let UIKeyboardWillChangeFrameNotification: String
@available(iOS 5.0, *)
let UIKeyboardDidChangeFrameNotification: String

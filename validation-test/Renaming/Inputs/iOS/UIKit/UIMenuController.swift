
enum UIMenuControllerArrowDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  @available(iOS 3.2, *)
  case up
  @available(iOS 3.2, *)
  case down
  @available(iOS 3.2, *)
  case left
  @available(iOS 3.2, *)
  case right
}
@available(iOS 3.0, *)
class UIMenuController : NSObject {
  @discardableResult
  class func shared() -> UIMenuController
  var isMenuVisible: Bool
  func setMenuVisible(_ menuVisible: Bool, animated animated: Bool)
  func setTargetRect(_ targetRect: CGRect, in targetView: UIView)
  @available(iOS 3.2, *)
  var arrowDirection: UIMenuControllerArrowDirection
  @available(iOS 3.2, *)
  var menuItems: [UIMenuItem]?
  func update()
  var menuFrame: CGRect { get }
}
let UIMenuControllerWillShowMenuNotification: String
let UIMenuControllerDidShowMenuNotification: String
let UIMenuControllerWillHideMenuNotification: String
let UIMenuControllerDidHideMenuNotification: String
let UIMenuControllerMenuFrameDidChangeNotification: String
@available(iOS 3.2, *)
class UIMenuItem : NSObject {
  init(title title: String, action action: Selector)
  var title: String
  var action: Selector
}

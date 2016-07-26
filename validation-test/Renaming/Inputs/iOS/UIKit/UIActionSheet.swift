
enum UIActionSheetStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case `default`
  case blackTranslucent
  case blackOpaque
}
@available(iOS, introduced: 2.0, deprecated: 8.3, message: "UIActionSheet is deprecated. Use UIAlertController with a preferredStyle of UIAlertControllerStyleActionSheet instead")
class UIActionSheet : UIView {
  init(title title: String?, delegate delegate: UIActionSheetDelegate?, cancelButtonTitle cancelButtonTitle: String?, destructiveButtonTitle destructiveButtonTitle: String?)
  weak var delegate: @sil_weak UIActionSheetDelegate?
  var title: String
  var actionSheetStyle: UIActionSheetStyle
  @discardableResult
  func addButton(withTitle title: String?) -> Int
  @discardableResult
  func buttonTitle(at buttonIndex: Int) -> String?
  var numberOfButtons: Int { get }
  var cancelButtonIndex: Int
  var destructiveButtonIndex: Int
  var firstOtherButtonIndex: Int { get }
  var isVisible: Bool { get }
  func show(from view: UIToolbar)
  func show(from view: UITabBar)
  @available(iOS 3.2, *)
  func show(from item: UIBarButtonItem, animated animated: Bool)
  @available(iOS 3.2, *)
  func show(from rect: CGRect, in view: UIView, animated animated: Bool)
  func show(in view: UIView)
  func dismiss(withClickedButtonIndex buttonIndex: Int, animated animated: Bool)
}

extension UIActionSheet {
  convenience init(title title: String?, delegate delegate: UIActionSheetDelegate?, cancelButtonTitle cancelButtonTitle: String?, destructiveButtonTitle destructiveButtonTitle: String?, otherButtonTitles firstButtonTitle: String, _ moreButtonTitles: String...)
}
protocol UIActionSheetDelegate : NSObjectProtocol {
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func actionSheet(_ actionSheet: UIActionSheet, clickedButtonAt buttonIndex: Int)
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func actionSheetCancel(_ actionSheet: UIActionSheet)
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func willPresent(_ actionSheet: UIActionSheet)
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func didPresent(_ actionSheet: UIActionSheet)
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func actionSheet(_ actionSheet: UIActionSheet, willDismissWithButtonIndex buttonIndex: Int)
  @available(iOS, introduced: 2.0, deprecated: 8.3)
  optional func actionSheet(_ actionSheet: UIActionSheet, didDismissWithButtonIndex buttonIndex: Int)
}

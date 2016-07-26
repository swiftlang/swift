
enum UIButtonType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case custom
  @available(tvOS 7.0, *)
  case system
  case detailDisclosure
  case infoLight
  case infoDark
  case contactAdd
  static var roundedRect: UIButtonType { get }
}
@available(tvOS 2.0, *)
class UIButton : UIControl, NSCoding {
  convenience init(type buttonType: UIButtonType)
  var contentEdgeInsets: UIEdgeInsets
  var titleEdgeInsets: UIEdgeInsets
  var reversesTitleShadowWhenHighlighted: Bool
  var imageEdgeInsets: UIEdgeInsets
  var adjustsImageWhenHighlighted: Bool
  var adjustsImageWhenDisabled: Bool
  var buttonType: UIButtonType { get }
  func setTitle(_ title: String?, for state: UIControlState)
  func setTitleColor(_ color: UIColor?, for state: UIControlState)
  func setTitleShadowColor(_ color: UIColor?, for state: UIControlState)
  func setImage(_ image: UIImage?, for state: UIControlState)
  func setBackgroundImage(_ image: UIImage?, for state: UIControlState)
  @available(tvOS 6.0, *)
  func setAttributedTitle(_ title: NSAttributedString?, for state: UIControlState)
  @discardableResult
  func title(for state: UIControlState) -> String?
  @discardableResult
  func titleColor(for state: UIControlState) -> UIColor?
  @discardableResult
  func titleShadowColor(for state: UIControlState) -> UIColor?
  @discardableResult
  func image(for state: UIControlState) -> UIImage?
  @discardableResult
  func backgroundImage(for state: UIControlState) -> UIImage?
  @available(tvOS 6.0, *)
  @discardableResult
  func attributedTitle(for state: UIControlState) -> NSAttributedString?
  var currentTitle: String? { get }
  var currentTitleColor: UIColor { get }
  var currentTitleShadowColor: UIColor? { get }
  var currentImage: UIImage? { get }
  var currentBackgroundImage: UIImage? { get }
  @available(tvOS 6.0, *)
  var currentAttributedTitle: NSAttributedString? { get }
  @available(tvOS 3.0, *)
  var titleLabel: UILabel? { get }
  @available(tvOS 3.0, *)
  var imageView: UIImageView? { get }
  @discardableResult
  func backgroundRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func contentRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func titleRect(forContentRect contentRect: CGRect) -> CGRect
  @discardableResult
  func imageRect(forContentRect contentRect: CGRect) -> CGRect
}
extension UIButton {
}

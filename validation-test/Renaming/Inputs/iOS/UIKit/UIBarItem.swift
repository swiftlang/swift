
@available(iOS 2.0, *)
class UIBarItem : NSObject, NSCoding, UIAppearance {
  var isEnabled: Bool
  var title: String?
  var image: UIImage?
  @available(iOS 5.0, *)
  var landscapeImagePhone: UIImage?
  var imageInsets: UIEdgeInsets
  @available(iOS 5.0, *)
  var landscapeImagePhoneInsets: UIEdgeInsets
  var tag: Int
  @available(iOS 5.0, *)
  func setTitleTextAttributes(_ attributes: [String : AnyObject]?, for state: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func titleTextAttributes(for state: UIControlState) -> [String : AnyObject]?
}

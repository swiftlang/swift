
@available(tvOS 2.0, *)
class UIBarItem : NSObject, NSCoding, UIAppearance {
  var isEnabled: Bool
  var title: String?
  var image: UIImage?
  var imageInsets: UIEdgeInsets
  var tag: Int
  @available(tvOS 5.0, *)
  func setTitleTextAttributes(_ attributes: [String : AnyObject]?, for state: UIControlState)
  @available(tvOS 5.0, *)
  @discardableResult
  func titleTextAttributes(for state: UIControlState) -> [String : AnyObject]?
}

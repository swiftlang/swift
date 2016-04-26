
enum UITabBarSystemItem : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case more
  case favorites
  case featured
  case topRated
  case recents
  case contacts
  case history
  case bookmarks
  case search
  case downloads
  case mostRecent
  case mostViewed
}
@available(tvOS 2.0, *)
class UITabBarItem : UIBarItem {
  convenience init(title title: String?, image image: UIImage?, tag tag: Int)
  @available(tvOS 7.0, *)
  convenience init(title title: String?, image image: UIImage?, selectedImage selectedImage: UIImage?)
  convenience init(tabBarSystemItem systemItem: UITabBarSystemItem, tag tag: Int)
  @available(tvOS 7.0, *)
  var selectedImage: UIImage?
  var badgeValue: String?
  @available(tvOS 5.0, *)
  var titlePositionAdjustment: UIOffset
}

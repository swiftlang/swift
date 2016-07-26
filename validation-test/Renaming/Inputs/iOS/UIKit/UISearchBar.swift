
enum UISearchBarIcon : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case search
  case clear
  case bookmark
  case resultsList
}
@available(iOS 7.0, *)
enum UISearchBarStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case prominent
  case minimal
}
@available(iOS 2.0, *)
class UISearchBar : UIView, UIBarPositioning, UITextInputTraits {
  var barStyle: UIBarStyle
  weak var delegate: @sil_weak UISearchBarDelegate?
  var text: String?
  var prompt: String?
  var placeholder: String?
  var showsBookmarkButton: Bool
  var showsCancelButton: Bool
  @available(iOS 3.2, *)
  var showsSearchResultsButton: Bool
  @available(iOS 3.2, *)
  var isSearchResultsButtonSelected: Bool
  @available(iOS 3.0, *)
  func setShowsCancelButton(_ showsCancelButton: Bool, animated animated: Bool)
  @available(iOS 7.0, *)
  var barTintColor: UIColor?
  @available(iOS 7.0, *)
  var searchBarStyle: UISearchBarStyle
  @available(iOS 3.0, *)
  var isTranslucent: Bool
  @available(iOS 3.0, *)
  var scopeButtonTitles: [String]?
  @available(iOS 3.0, *)
  var selectedScopeButtonIndex: Int
  @available(iOS 3.0, *)
  var showsScopeBar: Bool
  @available(iOS 5.0, *)
  var backgroundImage: UIImage?
  @available(iOS 5.0, *)
  var scopeBarBackgroundImage: UIImage?
  @available(iOS 7.0, *)
  func setBackgroundImage(_ backgroundImage: UIImage?, for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics)
  @available(iOS 7.0, *)
  @discardableResult
  func backgroundImage(for barPosition: UIBarPosition, barMetrics barMetrics: UIBarMetrics) -> UIImage?
  @available(iOS 5.0, *)
  func setSearchFieldBackgroundImage(_ backgroundImage: UIImage?, for state: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func searchFieldBackgroundImage(for state: UIControlState) -> UIImage?
  @available(iOS 5.0, *)
  func setImage(_ iconImage: UIImage?, for icon: UISearchBarIcon, state state: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func image(for icon: UISearchBarIcon, state state: UIControlState) -> UIImage?
  @available(iOS 5.0, *)
  func setScopeBarButtonBackgroundImage(_ backgroundImage: UIImage?, for state: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func scopeBarButtonBackgroundImage(for state: UIControlState) -> UIImage?
  @available(iOS 5.0, *)
  func setScopeBarButtonDividerImage(_ dividerImage: UIImage?, forLeftSegmentState leftState: UIControlState, rightSegmentState rightState: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func scopeBarButtonDividerImage(forLeftSegmentState leftState: UIControlState, rightSegmentState rightState: UIControlState) -> UIImage?
  @available(iOS 5.0, *)
  func setScopeBarButtonTitleTextAttributes(_ attributes: [String : AnyObject]?, for state: UIControlState)
  @available(iOS 5.0, *)
  @discardableResult
  func scopeBarButtonTitleTextAttributes(for state: UIControlState) -> [String : AnyObject]?
  @available(iOS 5.0, *)
  var searchFieldBackgroundPositionAdjustment: UIOffset
  @available(iOS 5.0, *)
  var searchTextPositionAdjustment: UIOffset
  @available(iOS 5.0, *)
  func setPositionAdjustment(_ adjustment: UIOffset, for icon: UISearchBarIcon)
  @available(iOS 5.0, *)
  @discardableResult
  func positionAdjustment(for icon: UISearchBarIcon) -> UIOffset
}
protocol UISearchBarDelegate : UIBarPositioningDelegate {
  @available(iOS 2.0, *)
  @discardableResult
  optional func searchBarShouldBeginEditing(_ searchBar: UISearchBar) -> Bool
  @available(iOS 2.0, *)
  optional func searchBarTextDidBeginEditing(_ searchBar: UISearchBar)
  @available(iOS 2.0, *)
  @discardableResult
  optional func searchBarShouldEndEditing(_ searchBar: UISearchBar) -> Bool
  @available(iOS 2.0, *)
  optional func searchBarTextDidEndEditing(_ searchBar: UISearchBar)
  @available(iOS 2.0, *)
  optional func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String)
  @available(iOS 3.0, *)
  @discardableResult
  optional func searchBar(_ searchBar: UISearchBar, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool
  @available(iOS 2.0, *)
  optional func searchBarSearchButtonClicked(_ searchBar: UISearchBar)
  @available(iOS 2.0, *)
  optional func searchBarBookmarkButtonClicked(_ searchBar: UISearchBar)
  @available(iOS 2.0, *)
  optional func searchBarCancelButtonClicked(_ searchBar: UISearchBar)
  @available(iOS 3.2, *)
  optional func searchBarResultsListButtonClicked(_ searchBar: UISearchBar)
  @available(iOS 3.0, *)
  optional func searchBar(_ searchBar: UISearchBar, selectedScopeButtonIndexDidChange selectedScope: Int)
}

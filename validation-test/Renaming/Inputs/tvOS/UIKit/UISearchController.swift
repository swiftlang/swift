
protocol UISearchControllerDelegate : NSObjectProtocol {
  @available(tvOS 8.0, *)
  optional func willPresent(_ searchController: UISearchController)
  @available(tvOS 8.0, *)
  optional func didPresent(_ searchController: UISearchController)
  @available(tvOS 8.0, *)
  optional func willDismiss(_ searchController: UISearchController)
  @available(tvOS 8.0, *)
  optional func didDismiss(_ searchController: UISearchController)
  @available(tvOS 8.0, *)
  optional func present(_ searchController: UISearchController)
}
protocol UISearchResultsUpdating : NSObjectProtocol {
  @available(tvOS 8.0, *)
  func updateSearchResults(for searchController: UISearchController)
}
@available(tvOS 8.0, *)
class UISearchController : UIViewController, UIViewControllerTransitioningDelegate, UIViewControllerAnimatedTransitioning {
  init(searchResultsController searchResultsController: UIViewController?)
  weak var searchResultsUpdater: @sil_weak UISearchResultsUpdating?
  var isActive: Bool
  weak var delegate: @sil_weak UISearchControllerDelegate?
  @available(tvOS 9.1, *)
  var obscuresBackgroundDuringPresentation: Bool
  var hidesNavigationBarDuringPresentation: Bool
  var searchResultsController: UIViewController? { get }
  var searchBar: UISearchBar { get }
}

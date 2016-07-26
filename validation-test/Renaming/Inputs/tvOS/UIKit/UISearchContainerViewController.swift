
@available(tvOS 9.0, *)
class UISearchContainerViewController : UIViewController {
  var searchController: UISearchController { get }
  init(searchController searchController: UISearchController)
}

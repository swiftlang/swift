
@available(tvOS 6.0, *)
class UICollectionViewController : UIViewController, UICollectionViewDelegate, UICollectionViewDataSource {
  init(collectionViewLayout layout: UICollectionViewLayout)
  var collectionView: UICollectionView?
  var clearsSelectionOnViewWillAppear: Bool
  @available(tvOS 7.0, *)
  var useLayoutToLayoutNavigationTransitions: Bool
  @available(tvOS 7.0, *)
  var collectionViewLayout: UICollectionViewLayout { get }
  @available(tvOS 9.0, *)
  var installsStandardGestureForInteractiveMovement: Bool
}

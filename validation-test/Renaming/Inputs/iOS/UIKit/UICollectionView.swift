
struct UICollectionViewScrollPosition : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var top: UICollectionViewScrollPosition { get }
  static var centeredVertically: UICollectionViewScrollPosition { get }
  static var bottom: UICollectionViewScrollPosition { get }
  static var left: UICollectionViewScrollPosition { get }
  static var centeredHorizontally: UICollectionViewScrollPosition { get }
  static var right: UICollectionViewScrollPosition { get }
}
typealias UICollectionViewLayoutInteractiveTransitionCompletion = (Bool, Bool) -> Void
@available(iOS 9.0, *)
class UICollectionViewFocusUpdateContext : UIFocusUpdateContext {
  var previouslyFocusedIndexPath: NSIndexPath? { get }
  var nextFocusedIndexPath: NSIndexPath? { get }
}
protocol UICollectionViewDataSource : NSObjectProtocol {
  @available(iOS 6.0, *)
  @discardableResult
  func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int
  @available(iOS 6.0, *)
  @discardableResult
  func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: NSIndexPath) -> UICollectionViewCell
  @available(iOS 6.0, *)
  @discardableResult
  optional func numberOfSections(in collectionView: UICollectionView) -> Int
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: NSIndexPath) -> UICollectionReusableView
  @available(iOS 9.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, canMoveItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 9.0, *)
  optional func collectionView(_ collectionView: UICollectionView, moveItemAt sourceIndexPath: NSIndexPath, to destinationIndexPath: NSIndexPath)
}
protocol UICollectionViewDelegate : UIScrollViewDelegate {
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, shouldHighlightItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didHighlightItemAt indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didUnhighlightItemAt indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, shouldSelectItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, shouldDeselectItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didDeselectItemAt indexPath: NSIndexPath)
  @available(iOS 8.0, *)
  optional func collectionView(_ collectionView: UICollectionView, willDisplay cell: UICollectionViewCell, forItemAt indexPath: NSIndexPath)
  @available(iOS 8.0, *)
  optional func collectionView(_ collectionView: UICollectionView, willDisplaySupplementaryView view: UICollectionReusableView, forElementKind elementKind: String, at indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didEndDisplaying cell: UICollectionViewCell, forItemAt indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didEndDisplayingSupplementaryView view: UICollectionReusableView, forElementOfKind elementKind: String, at indexPath: NSIndexPath)
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, shouldShowMenuForItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, canPerformAction action: Selector, forItemAt indexPath: NSIndexPath, withSender sender: AnyObject?) -> Bool
  @available(iOS 6.0, *)
  optional func collectionView(_ collectionView: UICollectionView, performAction action: Selector, forItemAt indexPath: NSIndexPath, withSender sender: AnyObject?)
  @available(iOS 7.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, transitionLayoutForOldLayout fromLayout: UICollectionViewLayout, newLayout toLayout: UICollectionViewLayout) -> UICollectionViewTransitionLayout
  @available(iOS 9.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, canFocusItemAt indexPath: NSIndexPath) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, shouldUpdateFocusIn context: UICollectionViewFocusUpdateContext) -> Bool
  @available(iOS 9.0, *)
  optional func collectionView(_ collectionView: UICollectionView, didUpdateFocusIn context: UICollectionViewFocusUpdateContext, with coordinator: UIFocusAnimationCoordinator)
  @available(iOS 9.0, *)
  @discardableResult
  optional func indexPathForPreferredFocusedView(in collectionView: UICollectionView) -> NSIndexPath?
  @available(iOS 9.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, targetIndexPathForMoveFromItemAt originalIndexPath: NSIndexPath, toProposedIndexPath proposedIndexPath: NSIndexPath) -> NSIndexPath
  @available(iOS 9.0, *)
  @discardableResult
  optional func collectionView(_ collectionView: UICollectionView, targetContentOffsetForProposedContentOffset proposedContentOffset: CGPoint) -> CGPoint
}
@available(iOS 6.0, *)
class UICollectionView : UIScrollView {
  init(frame frame: CGRect, collectionViewLayout layout: UICollectionViewLayout)
  var collectionViewLayout: UICollectionViewLayout
  weak var dataSource: @sil_weak UICollectionViewDataSource?
  var backgroundView: UIView?
  func register(_ cellClass: AnyClass?, forCellWithReuseIdentifier identifier: String)
  func register(_ nib: UINib?, forCellWithReuseIdentifier identifier: String)
  func register(_ viewClass: AnyClass?, forSupplementaryViewOfKind elementKind: String, withReuseIdentifier identifier: String)
  func register(_ nib: UINib?, forSupplementaryViewOfKind kind: String, withReuseIdentifier identifier: String)
  @discardableResult
  func dequeueReusableCell(withReuseIdentifier identifier: String, for indexPath: NSIndexPath) -> UICollectionViewCell
  @discardableResult
  func dequeueReusableSupplementaryView(ofKind elementKind: String, withReuseIdentifier identifier: String, for indexPath: NSIndexPath) -> UICollectionReusableView
  var allowsSelection: Bool
  var allowsMultipleSelection: Bool
  @discardableResult
  func indexPathsForSelectedItems() -> [NSIndexPath]?
  func selectItem(at indexPath: NSIndexPath?, animated animated: Bool, scrollPosition scrollPosition: UICollectionViewScrollPosition)
  func deselectItem(at indexPath: NSIndexPath, animated animated: Bool)
  func reloadData()
  func setCollectionViewLayout(_ layout: UICollectionViewLayout, animated animated: Bool)
  @available(iOS 7.0, *)
  func setCollectionViewLayout(_ layout: UICollectionViewLayout, animated animated: Bool, completion completion: ((Bool) -> Void)? = nil)
  @available(iOS 7.0, *)
  @discardableResult
  func startInteractiveTransition(to layout: UICollectionViewLayout, completion completion: UICollectionViewLayoutInteractiveTransitionCompletion? = nil) -> UICollectionViewTransitionLayout
  @available(iOS 7.0, *)
  func finishInteractiveTransition()
  @available(iOS 7.0, *)
  func cancelInteractiveTransition()
  @discardableResult
  func numberOfSections() -> Int
  @discardableResult
  func numberOfItems(inSection section: Int) -> Int
  @discardableResult
  func layoutAttributesForItem(at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForSupplementaryElement(ofKind kind: String, at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func indexPathForItem(at point: CGPoint) -> NSIndexPath?
  @discardableResult
  func indexPath(for cell: UICollectionViewCell) -> NSIndexPath?
  @discardableResult
  func cellForItem(at indexPath: NSIndexPath) -> UICollectionViewCell?
  @discardableResult
  func visibleCells() -> [UICollectionViewCell]
  @discardableResult
  func indexPathsForVisibleItems() -> [NSIndexPath]
  @available(iOS 9.0, *)
  @discardableResult
  func supplementaryView(forElementKind elementKind: String, at indexPath: NSIndexPath) -> UICollectionReusableView
  @available(iOS 9.0, *)
  @discardableResult
  func visibleSupplementaryViews(ofKind elementKind: String) -> [UICollectionReusableView]
  @available(iOS 9.0, *)
  @discardableResult
  func indexPathsForVisibleSupplementaryElements(ofKind elementKind: String) -> [NSIndexPath]
  func scrollToItem(at indexPath: NSIndexPath, at scrollPosition: UICollectionViewScrollPosition, animated animated: Bool)
  func insertSections(_ sections: NSIndexSet)
  func deleteSections(_ sections: NSIndexSet)
  func reloadSections(_ sections: NSIndexSet)
  func moveSection(_ section: Int, toSection newSection: Int)
  func insertItems(at indexPaths: [NSIndexPath])
  func deleteItems(at indexPaths: [NSIndexPath])
  func reloadItems(at indexPaths: [NSIndexPath])
  func moveItem(at indexPath: NSIndexPath, to newIndexPath: NSIndexPath)
  func performBatchUpdates(_ updates: (() -> Void)?, completion completion: ((Bool) -> Void)? = nil)
  @available(iOS 9.0, *)
  @discardableResult
  func beginInteractiveMovementForItem(at indexPath: NSIndexPath) -> Bool
  @available(iOS 9.0, *)
  func updateInteractiveMovementTargetPosition(_ targetPosition: CGPoint)
  @available(iOS 9.0, *)
  func endInteractiveMovement()
  @available(iOS 9.0, *)
  func cancelInteractiveMovement()
  @available(iOS 9.0, *)
  var remembersLastFocusedIndexPath: Bool
}
extension NSIndexPath {
  @available(iOS 6.0, *)
  convenience init(forItem item: Int, inSection section: Int)
  @available(iOS 6.0, *)
  var item: Int { get }
}

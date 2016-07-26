
enum UICollectionElementCategory : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case cell
  case supplementaryView
  case decorationView
}
@available(iOS 6.0, *)
class UICollectionViewLayoutAttributes : NSObject, NSCopying, UIDynamicItem {
  var frame: CGRect
  var size: CGSize
  var transform3D: CATransform3D
  var alpha: CGFloat
  var zIndex: Int
  var isHidden: Bool
  var indexPath: NSIndexPath
  var representedElementCategory: UICollectionElementCategory { get }
  var representedElementKind: String? { get }
  convenience init(forCellWith indexPath: NSIndexPath)
  convenience init(forSupplementaryViewOfKind elementKind: String, with indexPath: NSIndexPath)
  convenience init(forDecorationViewOfKind decorationViewKind: String, with indexPath: NSIndexPath)
}
enum UICollectionUpdateAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case insert
  case delete
  case reload
  case move
  case none
}
@available(iOS 6.0, *)
class UICollectionViewUpdateItem : NSObject {
  var indexPathBeforeUpdate: NSIndexPath? { get }
  var indexPathAfterUpdate: NSIndexPath? { get }
  var updateAction: UICollectionUpdateAction { get }
}
@available(iOS 7.0, *)
class UICollectionViewLayoutInvalidationContext : NSObject {
  var invalidateEverything: Bool { get }
  var invalidateDataSourceCounts: Bool { get }
  @available(iOS 8.0, *)
  func invalidateItems(at indexPaths: [NSIndexPath])
  @available(iOS 8.0, *)
  func invalidateSupplementaryElements(ofKind elementKind: String, at indexPaths: [NSIndexPath])
  @available(iOS 8.0, *)
  func invalidateDecorationElements(ofKind elementKind: String, at indexPaths: [NSIndexPath])
  @available(iOS 8.0, *)
  var invalidatedItemIndexPaths: [NSIndexPath]? { get }
  @available(iOS 8.0, *)
  var invalidatedSupplementaryIndexPaths: [String : [NSIndexPath]]? { get }
  @available(iOS 8.0, *)
  var invalidatedDecorationIndexPaths: [String : [NSIndexPath]]? { get }
  @available(iOS 8.0, *)
  var contentOffsetAdjustment: CGPoint
  @available(iOS 8.0, *)
  var contentSizeAdjustment: CGSize
  @available(iOS 9.0, *)
  var previousIndexPathsForInteractivelyMovingItems: [NSIndexPath]? { get }
  @available(iOS 9.0, *)
  var targetIndexPathsForInteractivelyMovingItems: [NSIndexPath]? { get }
  @available(iOS 9.0, *)
  var interactiveMovementTarget: CGPoint { get }
}
@available(iOS 6.0, *)
class UICollectionViewLayout : NSObject, NSCoding {
  var collectionView: UICollectionView? { get }
  func invalidateLayout()
  @available(iOS 7.0, *)
  func invalidateLayout(with context: UICollectionViewLayoutInvalidationContext)
  func register(_ viewClass: AnyClass?, forDecorationViewOfKind elementKind: String)
  func register(_ nib: UINib?, forDecorationViewOfKind elementKind: String)
}
extension UICollectionViewLayout {
  @discardableResult
  class func layoutAttributesClass() -> AnyClass
  @available(iOS 7.0, *)
  @discardableResult
  class func invalidationContextClass() -> AnyClass
  func prepare()
  @discardableResult
  func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]?
  @discardableResult
  func layoutAttributesForItem(at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForSupplementaryView(ofKind elementKind: String, at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForDecorationView(ofKind elementKind: String, at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func shouldInvalidateLayout(forBoundsChange newBounds: CGRect) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  func invalidationContext(forBoundsChange newBounds: CGRect) -> UICollectionViewLayoutInvalidationContext
  @available(iOS 8.0, *)
  @discardableResult
  func shouldInvalidateLayout(forPreferredLayoutAttributes preferredAttributes: UICollectionViewLayoutAttributes, withOriginalAttributes originalAttributes: UICollectionViewLayoutAttributes) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  func invalidationContext(forPreferredLayoutAttributes preferredAttributes: UICollectionViewLayoutAttributes, withOriginalAttributes originalAttributes: UICollectionViewLayoutAttributes) -> UICollectionViewLayoutInvalidationContext
  @discardableResult
  func targetContentOffset(forProposedContentOffset proposedContentOffset: CGPoint, withScrollingVelocity velocity: CGPoint) -> CGPoint
  @available(iOS 7.0, *)
  @discardableResult
  func targetContentOffset(forProposedContentOffset proposedContentOffset: CGPoint) -> CGPoint
  @discardableResult
  func collectionViewContentSize() -> CGSize
}
extension UICollectionViewLayout {
  func prepare(forCollectionViewUpdates updateItems: [UICollectionViewUpdateItem])
  func finalizeCollectionViewUpdates()
  func prepare(forAnimatedBoundsChange oldBounds: CGRect)
  func finalizeAnimatedBoundsChange()
  @available(iOS 7.0, *)
  func prepareForTransition(to newLayout: UICollectionViewLayout)
  @available(iOS 7.0, *)
  func prepareForTransition(from oldLayout: UICollectionViewLayout)
  @available(iOS 7.0, *)
  func finalizeLayoutTransition()
  @discardableResult
  func initialLayoutAttributesForAppearingItem(at itemIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingItem(at itemIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func initialLayoutAttributesForAppearingSupplementaryElement(ofKind elementKind: String, at elementIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingSupplementaryElement(ofKind elementKind: String, at elementIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func initialLayoutAttributesForAppearingDecorationElement(ofKind elementKind: String, at decorationIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingDecorationElement(ofKind elementKind: String, at decorationIndexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @available(iOS 7.0, *)
  @discardableResult
  func indexPathsToDeleteForSupplementaryView(ofKind elementKind: String) -> [NSIndexPath]
  @available(iOS 7.0, *)
  @discardableResult
  func indexPathsToDeleteForDecorationView(ofKind elementKind: String) -> [NSIndexPath]
  @available(iOS 7.0, *)
  @discardableResult
  func indexPathsToInsertForSupplementaryView(ofKind elementKind: String) -> [NSIndexPath]
  @available(iOS 7.0, *)
  @discardableResult
  func indexPathsToInsertForDecorationView(ofKind elementKind: String) -> [NSIndexPath]
}
extension UICollectionViewLayout {
  @available(iOS 9.0, *)
  @discardableResult
  func targetIndexPath(forInteractivelyMovingItem previousIndexPath: NSIndexPath, withPosition position: CGPoint) -> NSIndexPath
  @available(iOS 9.0, *)
  @discardableResult
  func layoutAttributesForInteractivelyMovingItem(at indexPath: NSIndexPath, withTargetPosition position: CGPoint) -> UICollectionViewLayoutAttributes
  @available(iOS 9.0, *)
  @discardableResult
  func invalidationContext(forInteractivelyMovingItems targetIndexPaths: [NSIndexPath], withTargetPosition targetPosition: CGPoint, previousIndexPaths previousIndexPaths: [NSIndexPath], previousPosition previousPosition: CGPoint) -> UICollectionViewLayoutInvalidationContext
  @available(iOS 9.0, *)
  @discardableResult
  func invalidationContextForEndingInteractiveMovementOfItems(toFinalIndexPaths indexPaths: [NSIndexPath], previousIndexPaths previousIndexPaths: [NSIndexPath], movementCancelled movementCancelled: Bool) -> UICollectionViewLayoutInvalidationContext
}

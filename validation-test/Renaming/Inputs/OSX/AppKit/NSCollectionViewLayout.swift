
@available(OSX 10.11, *)
enum NSCollectionElementCategory : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case item
  case supplementaryView
  case decorationView
  case interItemGap
}
@available(OSX 10.11, *)
let NSCollectionElementKindInterItemGapIndicator: String
@available(OSX 10.11, *)
class NSCollectionViewLayoutAttributes : NSObject, NSCopying {
  var frame: NSRect
  var size: NSSize
  var alpha: CGFloat
  var zIndex: Int
  var isHidden: Bool
  var indexPath: NSIndexPath?
  var representedElementCategory: NSCollectionElementCategory { get }
  var representedElementKind: String? { get }
  convenience init(forItemWith indexPath: NSIndexPath)
  convenience init(forInterItemGapBefore indexPath: NSIndexPath)
  convenience init(forSupplementaryViewOfKind elementKind: String, with indexPath: NSIndexPath)
  convenience init(forDecorationViewOfKind decorationViewKind: String, with indexPath: NSIndexPath)
}
@available(OSX 10.11, *)
enum NSCollectionUpdateAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case insert
  case delete
  case reload
  case move
  case none
}
@available(OSX 10.11, *)
class NSCollectionViewUpdateItem : NSObject {
  var indexPathBeforeUpdate: NSIndexPath? { get }
  var indexPathAfterUpdate: NSIndexPath? { get }
  var updateAction: NSCollectionUpdateAction { get }
}
@available(OSX 10.11, *)
class NSCollectionViewLayoutInvalidationContext : NSObject {
  var invalidateEverything: Bool { get }
  var invalidateDataSourceCounts: Bool { get }
  func invalidateItems(at indexPaths: Set<NSIndexPath>)
  func invalidateSupplementaryElements(ofKind elementKind: String, at indexPaths: Set<NSIndexPath>)
  func invalidateDecorationElements(ofKind elementKind: String, at indexPaths: Set<NSIndexPath>)
  var invalidatedItemIndexPaths: Set<NSIndexPath>? { get }
  var invalidatedSupplementaryIndexPaths: [String : Set<NSIndexPath>]? { get }
  var invalidatedDecorationIndexPaths: [String : Set<NSIndexPath>]? { get }
  var contentOffsetAdjustment: NSPoint
  var contentSizeAdjustment: NSSize
}
@available(OSX 10.11, *)
class NSCollectionViewLayout : NSObject, NSCoding {
  weak var collectionView: @sil_weak NSCollectionView? { get }
  func invalidateLayout()
  func invalidateLayout(with context: NSCollectionViewLayoutInvalidationContext)
  func register(_ viewClass: AnyClass?, forDecorationViewOfKind elementKind: String)
  func register(_ nib: NSNib?, forDecorationViewOfKind elementKind: String)
}
extension NSCollectionViewLayout {
  @discardableResult
  class func layoutAttributesClass() -> AnyClass
  @discardableResult
  class func invalidationContextClass() -> AnyClass
  func prepare()
  @discardableResult
  func layoutAttributesForElements(in rect: NSRect) -> [NSCollectionViewLayoutAttributes]
  @discardableResult
  func layoutAttributesForItem(at indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForSupplementaryView(ofKind elementKind: String, at indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForDecorationView(ofKind elementKind: String, at indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForDropTarget(at pointInCollectionView: NSPoint) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForInterItemGap(before indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func shouldInvalidateLayout(forBoundsChange newBounds: NSRect) -> Bool
  @discardableResult
  func invalidationContext(forBoundsChange newBounds: NSRect) -> NSCollectionViewLayoutInvalidationContext
  @discardableResult
  func shouldInvalidateLayout(forPreferredLayoutAttributes preferredAttributes: NSCollectionViewLayoutAttributes, withOriginalAttributes originalAttributes: NSCollectionViewLayoutAttributes) -> Bool
  @discardableResult
  func invalidationContext(forPreferredLayoutAttributes preferredAttributes: NSCollectionViewLayoutAttributes, withOriginalAttributes originalAttributes: NSCollectionViewLayoutAttributes) -> NSCollectionViewLayoutInvalidationContext
  @discardableResult
  func targetContentOffset(forProposedContentOffset proposedContentOffset: NSPoint, withScrollingVelocity velocity: NSPoint) -> NSPoint
  @discardableResult
  func targetContentOffset(forProposedContentOffset proposedContentOffset: NSPoint) -> NSPoint
  var collectionViewContentSize: NSSize { get }
}
extension NSCollectionViewLayout {
  func prepare(forCollectionViewUpdates updateItems: [NSCollectionViewUpdateItem])
  func finalizeCollectionViewUpdates()
  func prepare(forAnimatedBoundsChange oldBounds: NSRect)
  func finalizeAnimatedBoundsChange()
  func prepareForTransition(to newLayout: NSCollectionViewLayout)
  func prepareForTransition(from oldLayout: NSCollectionViewLayout)
  func finalizeLayoutTransition()
  @discardableResult
  func initialLayoutAttributesForAppearingItem(at itemIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingItem(at itemIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func initialLayoutAttributesForAppearingSupplementaryElement(ofKind elementKind: String, at elementIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingSupplementaryElement(ofKind elementKind: String, at elementIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func initialLayoutAttributesForAppearingDecorationElement(ofKind elementKind: String, at decorationIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func finalLayoutAttributesForDisappearingDecorationElement(ofKind elementKind: String, at decorationIndexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @discardableResult
  func indexPathsToDeleteForSupplementaryView(ofKind elementKind: String) -> Set<NSIndexPath>
  @discardableResult
  func indexPathsToDeleteForDecorationView(ofKind elementKind: String) -> Set<NSIndexPath>
  @discardableResult
  func indexPathsToInsertForSupplementaryView(ofKind elementKind: String) -> Set<NSIndexPath>
  @discardableResult
  func indexPathsToInsertForDecorationView(ofKind elementKind: String) -> Set<NSIndexPath>
}

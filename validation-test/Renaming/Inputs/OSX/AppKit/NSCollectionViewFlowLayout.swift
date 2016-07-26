
@available(OSX 10.11, *)
enum NSCollectionViewScrollDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case vertical
  case horizontal
}
@available(OSX 10.11, *)
let NSCollectionElementKindSectionHeader: String
@available(OSX 10.11, *)
let NSCollectionElementKindSectionFooter: String
@available(OSX 10.11, *)
class NSCollectionViewFlowLayoutInvalidationContext : NSCollectionViewLayoutInvalidationContext {
  var invalidateFlowLayoutDelegateMetrics: Bool
  var invalidateFlowLayoutAttributes: Bool
}
protocol NSCollectionViewDelegateFlowLayout : NSCollectionViewDelegate {
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, sizeForItemAt indexPath: NSIndexPath) -> NSSize
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, insetForSectionAt section: Int) -> NSEdgeInsets
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, minimumLineSpacingForSectionAt section: Int) -> CGFloat
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, minimumInteritemSpacingForSectionAt section: Int) -> CGFloat
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, referenceSizeForHeaderInSection section: Int) -> NSSize
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout, referenceSizeForFooterInSection section: Int) -> NSSize
}
@available(OSX 10.11, *)
class NSCollectionViewFlowLayout : NSCollectionViewLayout {
  var minimumLineSpacing: CGFloat
  var minimumInteritemSpacing: CGFloat
  var itemSize: NSSize
  var estimatedItemSize: NSSize
  var scrollDirection: NSCollectionViewScrollDirection
  var headerReferenceSize: NSSize
  var footerReferenceSize: NSSize
  var sectionInset: NSEdgeInsets
}

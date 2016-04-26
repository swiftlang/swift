
protocol UIDynamicAnimatorDelegate : NSObjectProtocol {
  @available(tvOS 7.0, *)
  optional func dynamicAnimatorWillResume(_ animator: UIDynamicAnimator)
  @available(tvOS 7.0, *)
  optional func dynamicAnimatorDidPause(_ animator: UIDynamicAnimator)
}
@available(tvOS 7.0, *)
class UIDynamicAnimator : NSObject {
  init(referenceView view: UIView)
  func addBehavior(_ behavior: UIDynamicBehavior)
  func removeBehavior(_ behavior: UIDynamicBehavior)
  func removeAllBehaviors()
  var referenceView: UIView? { get }
  var behaviors: [UIDynamicBehavior] { get }
  @discardableResult
  func items(in rect: CGRect) -> [UIDynamicItem]
  func updateItem(usingCurrentState item: UIDynamicItem)
  var isRunning: Bool { get }
  @discardableResult
  func elapsedTime() -> NSTimeInterval
  weak var delegate: @sil_weak UIDynamicAnimatorDelegate?
}
extension UIDynamicAnimator {
  convenience init(collectionViewLayout layout: UICollectionViewLayout)
  @discardableResult
  func layoutAttributesForCell(at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForSupplementaryView(ofKind kind: String, at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
  @discardableResult
  func layoutAttributesForDecorationView(ofKind decorationViewKind: String, at indexPath: NSIndexPath) -> UICollectionViewLayoutAttributes?
}

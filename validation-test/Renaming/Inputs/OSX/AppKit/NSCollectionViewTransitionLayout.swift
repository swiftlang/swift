
@available(OSX 10.11, *)
class NSCollectionViewTransitionLayout : NSCollectionViewLayout {
  var transitionProgress: CGFloat
  var currentLayout: NSCollectionViewLayout { get }
  var nextLayout: NSCollectionViewLayout { get }
  init(currentLayout currentLayout: NSCollectionViewLayout, nextLayout newLayout: NSCollectionViewLayout)
  func updateValue(_ value: CGFloat, forAnimatedKey key: String)
  @discardableResult
  func value(forAnimatedKey key: String) -> CGFloat
}

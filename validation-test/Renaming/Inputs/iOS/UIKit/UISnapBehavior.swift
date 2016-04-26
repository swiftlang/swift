
@available(iOS 7.0, *)
class UISnapBehavior : UIDynamicBehavior {
  init(item item: UIDynamicItem, snapTo point: CGPoint)
  @available(iOS 9.0, *)
  var snapPoint: CGPoint
  var damping: CGFloat
}

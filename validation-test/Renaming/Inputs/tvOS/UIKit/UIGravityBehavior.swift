
@available(tvOS 7.0, *)
class UIGravityBehavior : UIDynamicBehavior {
  init(items items: [UIDynamicItem])
  func addItem(_ item: UIDynamicItem)
  func removeItem(_ item: UIDynamicItem)
  var items: [UIDynamicItem] { get }
  var gravityDirection: CGVector
  var angle: CGFloat
  var magnitude: CGFloat
  func setAngle(_ angle: CGFloat, magnitude magnitude: CGFloat)
}

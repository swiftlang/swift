
@available(tvOS 7.0, *)
class UIDynamicItemBehavior : UIDynamicBehavior {
  init(items items: [UIDynamicItem])
  func addItem(_ item: UIDynamicItem)
  func removeItem(_ item: UIDynamicItem)
  var items: [UIDynamicItem] { get }
  var elasticity: CGFloat
  var friction: CGFloat
  var density: CGFloat
  var resistance: CGFloat
  var angularResistance: CGFloat
  @available(tvOS 9.0, *)
  var charge: CGFloat
  @available(tvOS 9.0, *)
  var isAnchored: Bool
  var allowsRotation: Bool
  func addLinearVelocity(_ velocity: CGPoint, for item: UIDynamicItem)
  @discardableResult
  func linearVelocity(for item: UIDynamicItem) -> CGPoint
  func addAngularVelocity(_ velocity: CGFloat, for item: UIDynamicItem)
  @discardableResult
  func angularVelocity(for item: UIDynamicItem) -> CGFloat
}

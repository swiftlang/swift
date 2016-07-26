
@available(tvOS 7.0, *)
enum UIPushBehaviorMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case continuous
  case instantaneous
}
@available(tvOS 7.0, *)
class UIPushBehavior : UIDynamicBehavior {
  init(items items: [UIDynamicItem], mode mode: UIPushBehaviorMode)
  func addItem(_ item: UIDynamicItem)
  func removeItem(_ item: UIDynamicItem)
  var items: [UIDynamicItem] { get }
  @discardableResult
  func targetOffsetFromCenter(for item: UIDynamicItem) -> UIOffset
  func setTargetOffsetFromCenter(_ o: UIOffset, for item: UIDynamicItem)
  var mode: UIPushBehaviorMode { get }
  var active: Bool
  var angle: CGFloat
  var magnitude: CGFloat
  var pushDirection: CGVector
  func setAngle(_ angle: CGFloat, magnitude magnitude: CGFloat)
}

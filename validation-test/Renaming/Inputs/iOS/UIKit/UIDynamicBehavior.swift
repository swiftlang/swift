
@available(iOS 9.0, *)
enum UIDynamicItemCollisionBoundsType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case rectangle
  case ellipse
  case path
}
protocol UIDynamicItem : NSObjectProtocol {
  var center: CGPoint { get set }
  var bounds: CGRect { get }
  var transform: CGAffineTransform { get set }
  @available(iOS 9.0, *)
  optional var collisionBoundsType: UIDynamicItemCollisionBoundsType { get }
  @available(iOS 9.0, *)
  optional var collisionBoundingPath: UIBezierPath { get }
}
@available(iOS 9.0, *)
class UIDynamicItemGroup : NSObject, UIDynamicItem {
  init(items items: [UIDynamicItem])
  var items: [UIDynamicItem] { get }
}
@available(iOS 7.0, *)
class UIDynamicBehavior : NSObject {
  func addChildBehavior(_ behavior: UIDynamicBehavior)
  func removeChildBehavior(_ behavior: UIDynamicBehavior)
  var childBehaviors: [UIDynamicBehavior] { get }
  var action: (() -> Void)?
  func willMove(to dynamicAnimator: UIDynamicAnimator?)
  var dynamicAnimator: UIDynamicAnimator? { get }
}

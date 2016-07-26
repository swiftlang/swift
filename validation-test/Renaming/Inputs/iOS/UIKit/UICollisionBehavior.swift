
@available(iOS 7.0, *)
struct UICollisionBehaviorMode : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var items: UICollisionBehaviorMode { get }
  static var boundaries: UICollisionBehaviorMode { get }
  static var everything: UICollisionBehaviorMode { get }
}
protocol UICollisionBehaviorDelegate : NSObjectProtocol {
  @available(iOS 7.0, *)
  optional func collisionBehavior(_ behavior: UICollisionBehavior, beganContactFor item1: UIDynamicItem, with item2: UIDynamicItem, at p: CGPoint)
  @available(iOS 7.0, *)
  optional func collisionBehavior(_ behavior: UICollisionBehavior, endedContactFor item1: UIDynamicItem, with item2: UIDynamicItem)
  @available(iOS 7.0, *)
  optional func collisionBehavior(_ behavior: UICollisionBehavior, beganContactFor item: UIDynamicItem, withBoundaryIdentifier identifier: NSCopying?, at p: CGPoint)
  @available(iOS 7.0, *)
  optional func collisionBehavior(_ behavior: UICollisionBehavior, endedContactFor item: UIDynamicItem, withBoundaryIdentifier identifier: NSCopying?)
}
@available(iOS 7.0, *)
class UICollisionBehavior : UIDynamicBehavior {
  init(items items: [UIDynamicItem])
  func addItem(_ item: UIDynamicItem)
  func removeItem(_ item: UIDynamicItem)
  var items: [UIDynamicItem] { get }
  var collisionMode: UICollisionBehaviorMode
  var translatesReferenceBoundsIntoBoundary: Bool
  func setTranslatesReferenceBoundsIntoBoundaryWith(_ insets: UIEdgeInsets)
  func addBoundary(withIdentifier identifier: NSCopying, for bezierPath: UIBezierPath)
  func addBoundary(withIdentifier identifier: NSCopying, from p1: CGPoint, to p2: CGPoint)
  @discardableResult
  func boundary(withIdentifier identifier: NSCopying) -> UIBezierPath?
  func removeBoundary(withIdentifier identifier: NSCopying)
  var boundaryIdentifiers: [NSCopying]? { get }
  func removeAllBoundaries()
  weak var collisionDelegate: @sil_weak UICollisionBehaviorDelegate?
}

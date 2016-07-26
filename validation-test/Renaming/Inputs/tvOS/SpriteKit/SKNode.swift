
@available(tvOS 7.0, *)
enum SKBlendMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case alpha
  case add
  case subtract
  case multiply
  case multiplyX2
  case screen
  case replace
}
class SKNode : UIResponder, NSCopying, NSCoding {
  convenience init?(fileNamed filename: String)
  var frame: CGRect { get }
  @discardableResult
  func calculateAccumulatedFrame() -> CGRect
  var position: CGPoint
  var zPosition: CGFloat
  var zRotation: CGFloat
  var xScale: CGFloat
  var yScale: CGFloat
  var speed: CGFloat
  var alpha: CGFloat
  var isPaused: Bool
  var isHidden: Bool
  var isUserInteractionEnabled: Bool
  var parent: SKNode? { get }
  var children: [SKNode] { get }
  var name: String?
  var scene: SKScene? { get }
  var physicsBody: SKPhysicsBody?
  var userData: NSMutableDictionary?
  @NSCopying var reachConstraints: SKReachConstraints?
  var constraints: [SKConstraint]?
  func setScale(_ scale: CGFloat)
  func addChild(_ node: SKNode)
  func insertChild(_ node: SKNode, at index: Int)
  func removeChildren(in nodes: [SKNode])
  func removeAllChildren()
  func removeFromParent()
  func move(toParent parent: SKNode)
  @discardableResult
  func childNode(withName name: String) -> SKNode?
  func enumerateChildNodes(withName name: String, using block: (SKNode, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 8.0, *)
  subscript(_ name: String) -> [SKNode] { get }
  @discardableResult
  func inParentHierarchy(_ parent: SKNode) -> Bool
  func run(_ action: SKAction)
  func run(_ action: SKAction, completion block: () -> Void)
  func run(_ action: SKAction, withKey key: String)
  @discardableResult
  func hasActions() -> Bool
  @discardableResult
  func action(forKey key: String) -> SKAction?
  func removeAction(forKey key: String)
  func removeAllActions()
  @discardableResult
  func contains(_ p: CGPoint) -> Bool
  @discardableResult
  func atPoint(_ p: CGPoint) -> SKNode
  @discardableResult
  func nodes(at p: CGPoint) -> [SKNode]
  @discardableResult
  func convert(_ point: CGPoint, from node: SKNode) -> CGPoint
  @discardableResult
  func convert(_ point: CGPoint, to node: SKNode) -> CGPoint
  @discardableResult
  func intersects(_ node: SKNode) -> Bool
  @discardableResult
  func isEqual(to node: SKNode) -> Bool
}
extension UITouch {
  @discardableResult
  func location(in node: SKNode) -> CGPoint
  @discardableResult
  func previousLocation(in node: SKNode) -> CGPoint
}

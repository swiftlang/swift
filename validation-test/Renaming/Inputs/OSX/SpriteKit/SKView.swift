
class SKView : NSView {
  var isPaused: Bool
  var showsFPS: Bool
  var showsDrawCount: Bool
  var showsNodeCount: Bool
  @available(OSX 10.10, *)
  var showsQuadCount: Bool
  @available(OSX 10.10, *)
  var showsPhysics: Bool
  @available(OSX 10.10, *)
  var showsFields: Bool
  var isAsynchronous: Bool
  @available(OSX 10.10, *)
  var allowsTransparency: Bool
  var ignoresSiblingOrder: Bool
  @available(OSX 10.10, *)
  var shouldCullNonVisibleNodes: Bool
  var frameInterval: Int
  func presentScene(_ scene: SKScene?)
  func presentScene(_ scene: SKScene, transition transition: SKTransition)
  var scene: SKScene? { get }
  @discardableResult
  func texture(from node: SKNode) -> SKTexture?
  @discardableResult
  func texture(from node: SKNode, crop crop: CGRect) -> SKTexture?
  @discardableResult
  func convert(_ point: CGPoint, to scene: SKScene) -> CGPoint
  @discardableResult
  func convert(_ point: CGPoint, from scene: SKScene) -> CGPoint
}

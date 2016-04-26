
@available(OSX 10.9, *)
enum SKSceneScaleMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fill
  case aspectFill
  case aspectFit
  case resizeFill
}
@available(OSX 10.10, *)
protocol SKSceneDelegate : NSObjectProtocol {
  optional func update(_ currentTime: NSTimeInterval, for scene: SKScene)
  optional func didEvaluateActions(for scene: SKScene)
  optional func didSimulatePhysics(for scene: SKScene)
  optional func didApplyConstraints(for scene: SKScene)
  optional func didFinishUpdate(for scene: SKScene)
}
class SKScene : SKEffectNode {
  init(size size: CGSize)
  var size: CGSize
  var scaleMode: SKSceneScaleMode
  @available(OSX 10.11, *)
  weak var camera: @sil_weak SKCameraNode?
  @available(OSX 10.11, *)
  weak var listener: @sil_weak SKNode?
  var backgroundColor: NSColor
  @available(OSX 10.10, *)
  unowned(unsafe) var delegate: @sil_unmanaged SKSceneDelegate?
  var anchorPoint: CGPoint
  var physicsWorld: SKPhysicsWorld { get }
  @discardableResult
  func convertPoint(fromView point: CGPoint) -> CGPoint
  @discardableResult
  func convertPoint(toView point: CGPoint) -> CGPoint
  weak var view: @sil_weak SKView? { get }
  func update(_ currentTime: NSTimeInterval)
  func didEvaluateActions()
  func didSimulatePhysics()
  @available(OSX 10.10, *)
  func didApplyConstraints()
  @available(OSX 10.10, *)
  func didFinishUpdate()
  func didMove(to view: SKView)
  func willMove(from view: SKView)
  func didChangeSize(_ oldSize: CGSize)
}

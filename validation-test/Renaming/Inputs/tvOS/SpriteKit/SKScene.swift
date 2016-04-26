
@available(tvOS 7.0, *)
enum SKSceneScaleMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fill
  case aspectFill
  case aspectFit
  case resizeFill
}
@available(tvOS 8.0, *)
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
  @available(tvOS 9.0, *)
  weak var camera: @sil_weak SKCameraNode?
  @available(tvOS 9.0, *)
  weak var listener: @sil_weak SKNode?
  var backgroundColor: UIColor
  @available(tvOS 8.0, *)
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
  @available(tvOS 8.0, *)
  func didApplyConstraints()
  @available(tvOS 8.0, *)
  func didFinishUpdate()
  func didMove(to view: SKView)
  func willMove(from view: SKView)
  func didChangeSize(_ oldSize: CGSize)
}

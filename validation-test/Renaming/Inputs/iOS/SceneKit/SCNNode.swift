
let SCNModelTransform: String
let SCNViewTransform: String
let SCNProjectionTransform: String
let SCNNormalTransform: String
let SCNModelViewTransform: String
let SCNModelViewProjectionTransform: String
@available(iOS 8.0, *)
class SCNNode : NSObject, NSCopying, NSSecureCoding, SCNAnimatable, SCNActionable, SCNBoundingVolume {
  /*not inherited*/ init(geometry geometry: SCNGeometry?)
  @discardableResult
  func clone() -> Self
  @available(iOS 8.0, *)
  @discardableResult
  func flattenedClone() -> Self
  var name: String?
  var light: SCNLight?
  var camera: SCNCamera?
  var geometry: SCNGeometry?
  @available(iOS 8.0, *)
  var skinner: SCNSkinner?
  @available(iOS 8.0, *)
  var morpher: SCNMorpher?
  var transform: SCNMatrix4
  var position: SCNVector3
  var rotation: SCNVector4
  @available(iOS 8.0, *)
  var orientation: SCNQuaternion
  @available(iOS 8.0, *)
  var eulerAngles: SCNVector3
  var scale: SCNVector3
  var pivot: SCNMatrix4
  var worldTransform: SCNMatrix4 { get }
  var isHidden: Bool
  var opacity: CGFloat
  var renderingOrder: Int
  @available(iOS 8.0, *)
  var castsShadow: Bool
  var parent: SCNNode? { get }
  var childNodes: [SCNNode] { get }
  func addChildNode(_ child: SCNNode)
  func insertChildNode(_ child: SCNNode, at index: Int)
  func removeFromParentNode()
  func replaceChildNode(_ oldChild: SCNNode, with newChild: SCNNode)
  @discardableResult
  func childNode(withName name: String, recursively recursively: Bool) -> SCNNode?
  @discardableResult
  func childNodes(passingTest predicate: (SCNNode, UnsafeMutablePointer<ObjCBool>) -> Bool) -> [SCNNode]
  @available(iOS 8.0, *)
  func enumerateChildNodes(_ block: (SCNNode, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(iOS 8.0, *)
  @discardableResult
  func convertPosition(_ position: SCNVector3, to node: SCNNode?) -> SCNVector3
  @available(iOS 8.0, *)
  @discardableResult
  func convertPosition(_ position: SCNVector3, from node: SCNNode?) -> SCNVector3
  @available(iOS 8.0, *)
  @discardableResult
  func convertTransform(_ transform: SCNMatrix4, to node: SCNNode?) -> SCNMatrix4
  @available(iOS 8.0, *)
  @discardableResult
  func convertTransform(_ transform: SCNMatrix4, from node: SCNNode?) -> SCNMatrix4
  @available(iOS 8.0, *)
  var physicsBody: SCNPhysicsBody?
  @available(iOS 8.0, *)
  var physicsField: SCNPhysicsField?
  @available(iOS 8.0, *)
  var constraints: [SCNConstraint]?
  @available(iOS 8.0, *)
  var filters: [CIFilter]?
  var presentation: SCNNode { get }
  @available(iOS 8.0, *)
  var isPaused: Bool
  unowned(unsafe) var rendererDelegate: @sil_unmanaged SCNNodeRendererDelegate?
  @available(iOS 8.0, *)
  @discardableResult
  func hitTestWithSegment(fromPoint pointA: SCNVector3, toPoint pointB: SCNVector3, options options: [String : AnyObject]? = [:]) -> [SCNHitTestResult]
  @available(iOS 8.0, *)
  var categoryBitMask: Int
}
protocol SCNNodeRendererDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func renderNode(_ node: SCNNode, renderer renderer: SCNRenderer, arguments arguments: [String : NSValue])
}

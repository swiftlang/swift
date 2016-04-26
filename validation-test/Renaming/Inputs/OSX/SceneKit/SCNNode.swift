
let SCNModelTransform: String
let SCNViewTransform: String
let SCNProjectionTransform: String
let SCNNormalTransform: String
let SCNModelViewTransform: String
let SCNModelViewProjectionTransform: String
@available(OSX 10.8, *)
class SCNNode : NSObject, NSCopying, NSSecureCoding, SCNAnimatable, SCNActionable, SCNBoundingVolume {
  /*not inherited*/ init(geometry geometry: SCNGeometry?)
  @discardableResult
  func clone() -> Self
  @available(OSX 10.9, *)
  @discardableResult
  func flattenedClone() -> Self
  var name: String?
  var light: SCNLight?
  var camera: SCNCamera?
  var geometry: SCNGeometry?
  @available(OSX 10.9, *)
  var skinner: SCNSkinner?
  @available(OSX 10.9, *)
  var morpher: SCNMorpher?
  var transform: SCNMatrix4
  var position: SCNVector3
  var rotation: SCNVector4
  @available(OSX 10.10, *)
  var orientation: SCNQuaternion
  @available(OSX 10.10, *)
  var eulerAngles: SCNVector3
  var scale: SCNVector3
  var pivot: SCNMatrix4
  var worldTransform: SCNMatrix4 { get }
  var isHidden: Bool
  var opacity: CGFloat
  var renderingOrder: Int
  @available(OSX 10.10, *)
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
  @available(OSX 10.10, *)
  func enumerateChildNodes(_ block: (SCNNode, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.9, *)
  @discardableResult
  func convertPosition(_ position: SCNVector3, to node: SCNNode?) -> SCNVector3
  @available(OSX 10.9, *)
  @discardableResult
  func convertPosition(_ position: SCNVector3, from node: SCNNode?) -> SCNVector3
  @available(OSX 10.9, *)
  @discardableResult
  func convertTransform(_ transform: SCNMatrix4, to node: SCNNode?) -> SCNMatrix4
  @available(OSX 10.9, *)
  @discardableResult
  func convertTransform(_ transform: SCNMatrix4, from node: SCNNode?) -> SCNMatrix4
  @available(OSX 10.10, *)
  var physicsBody: SCNPhysicsBody?
  @available(OSX 10.10, *)
  var physicsField: SCNPhysicsField?
  @available(OSX 10.9, *)
  var constraints: [SCNConstraint]?
  @available(OSX 10.9, *)
  var filters: [CIFilter]?
  var presentation: SCNNode { get }
  @available(OSX 10.10, *)
  var isPaused: Bool
  unowned(unsafe) var rendererDelegate: @sil_unmanaged SCNNodeRendererDelegate?
  @available(OSX 10.9, *)
  @discardableResult
  func hitTestWithSegment(fromPoint pointA: SCNVector3, toPoint pointB: SCNVector3, options options: [String : AnyObject]? = [:]) -> [SCNHitTestResult]
  @available(OSX 10.10, *)
  var categoryBitMask: Int
}
protocol SCNNodeRendererDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional func renderNode(_ node: SCNNode, renderer renderer: SCNRenderer, arguments arguments: [String : NSValue])
}

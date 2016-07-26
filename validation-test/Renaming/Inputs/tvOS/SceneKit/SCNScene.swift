
typealias SCNSceneExportProgressHandler = (Float, NSError?, UnsafeMutablePointer<ObjCBool>) -> Void
@available(tvOS 8.0, *)
let SCNSceneExportDestinationURL: String
let SCNSceneStartTimeAttributeKey: String
let SCNSceneEndTimeAttributeKey: String
let SCNSceneFrameRateAttributeKey: String
@available(tvOS 8.0, *)
let SCNSceneUpAxisAttributeKey: String
@available(tvOS 8.0, *)
class SCNScene : NSObject, NSSecureCoding {
  var rootNode: SCNNode { get }
  @available(tvOS 8.0, *)
  var physicsWorld: SCNPhysicsWorld { get }
  @discardableResult
  func attribute(forKey key: String) -> AnyObject?
  func setAttribute(_ attribute: AnyObject?, forKey key: String)
  @available(tvOS 8.0, *)
  var background: SCNMaterialProperty { get }
  @available(tvOS 8.0, *)
  convenience init?(named name: String)
  @available(tvOS 8.0, *)
  convenience init?(named name: String, inDirectory directory: String?, options options: [String : AnyObject]? = [:])
  convenience init(url url: NSURL, options options: [String : AnyObject]? = [:]) throws
  @available(tvOS 8.0, *)
  var fogStartDistance: CGFloat
  @available(tvOS 8.0, *)
  var fogEndDistance: CGFloat
  @available(tvOS 8.0, *)
  var fogDensityExponent: CGFloat
  @available(tvOS 8.0, *)
  var fogColor: AnyObject
  @available(tvOS 8.0, *)
  var isPaused: Bool
}

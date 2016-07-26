
typealias SCNSceneExportProgressHandler = (Float, NSError?, UnsafeMutablePointer<ObjCBool>) -> Void
@available(OSX 10.9, *)
let SCNSceneExportDestinationURL: String
let SCNSceneStartTimeAttributeKey: String
let SCNSceneEndTimeAttributeKey: String
let SCNSceneFrameRateAttributeKey: String
@available(OSX 10.10, *)
let SCNSceneUpAxisAttributeKey: String
@available(OSX 10.8, *)
class SCNScene : NSObject, NSSecureCoding {
  var rootNode: SCNNode { get }
  @available(OSX 10.10, *)
  var physicsWorld: SCNPhysicsWorld { get }
  @discardableResult
  func attribute(forKey key: String) -> AnyObject?
  func setAttribute(_ attribute: AnyObject?, forKey key: String)
  @available(OSX 10.9, *)
  var background: SCNMaterialProperty { get }
  @available(OSX 10.9, *)
  convenience init?(named name: String)
  @available(OSX 10.10, *)
  convenience init?(named name: String, inDirectory directory: String?, options options: [String : AnyObject]? = [:])
  convenience init(url url: NSURL, options options: [String : AnyObject]? = [:]) throws
  @available(OSX 10.9, *)
  @discardableResult
  func write(to url: NSURL, options options: [String : AnyObject]? = [:], delegate delegate: SCNSceneExportDelegate?, progressHandler progressHandler: SCNSceneExportProgressHandler? = nil) -> Bool
  @available(OSX 10.10, *)
  var fogStartDistance: CGFloat
  @available(OSX 10.10, *)
  var fogEndDistance: CGFloat
  @available(OSX 10.10, *)
  var fogDensityExponent: CGFloat
  @available(OSX 10.10, *)
  var fogColor: AnyObject
  @available(OSX 10.10, *)
  var isPaused: Bool
}
protocol SCNSceneExportDelegate : NSObjectProtocol {
  @available(OSX 10.9, *)
  @discardableResult
  optional func write(_ image: NSImage, withSceneDocumentURL documentURL: NSURL, originalImageURL originalImageURL: NSURL?) -> NSURL?
}

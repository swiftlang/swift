
let SCNHitTestFirstFoundOnlyKey: String
let SCNHitTestSortResultsKey: String
let SCNHitTestClipToZRangeKey: String
let SCNHitTestBackFaceCullingKey: String
let SCNHitTestBoundingBoxOnlyKey: String
let SCNHitTestIgnoreChildNodesKey: String
let SCNHitTestRootNodeKey: String
@available(OSX 10.9, *)
let SCNHitTestIgnoreHiddenNodesKey: String
@available(OSX 10.11, *)
enum SCNRenderingAPI : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case metal
  case openGLLegacy
  case openGLCore32
  case openGLCore41
}
@available(OSX 10.11, *)
struct SCNDebugOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var showPhysicsShapes: SCNDebugOptions { get }
  static var showBoundingBoxes: SCNDebugOptions { get }
  static var showLightInfluences: SCNDebugOptions { get }
  static var showLightExtents: SCNDebugOptions { get }
  static var showPhysicsFields: SCNDebugOptions { get }
  static var showWireframe: SCNDebugOptions { get }
}
@available(OSX 10.8, *)
class SCNHitTestResult : NSObject {
  var node: SCNNode { get }
  var geometryIndex: Int { get }
  var faceIndex: Int { get }
  var localCoordinates: SCNVector3 { get }
  var worldCoordinates: SCNVector3 { get }
  var localNormal: SCNVector3 { get }
  var worldNormal: SCNVector3 { get }
  var modelTransform: SCNMatrix4 { get }
  @discardableResult
  func textureCoordinates(withMappingChannel channel: Int) -> CGPoint
}
protocol SCNSceneRenderer : NSObjectProtocol {
  @available(OSX 10.8, *)
  var scene: SCNScene? { get set }
  @available(OSX 10.10, *)
  var sceneTime: NSTimeInterval { get set }
  unowned(unsafe) var delegate: @sil_unmanaged SCNSceneRendererDelegate? { get set }
  @available(OSX 10.8, *)
  @discardableResult
  func hitTest(_ point: CGPoint, options options: [String : AnyObject]? = [:]) -> [SCNHitTestResult]
  @available(OSX 10.9, *)
  @discardableResult
  func isNode(insideFrustum node: SCNNode, withPointOfView pointOfView: SCNNode) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  func nodesInsideFrustum(withPointOfView pointOfView: SCNNode) -> [SCNNode]
  @available(OSX 10.9, *)
  @discardableResult
  func projectPoint(_ point: SCNVector3) -> SCNVector3
  @available(OSX 10.9, *)
  @discardableResult
  func unprojectPoint(_ point: SCNVector3) -> SCNVector3
  var isPlaying: Bool { get set }
  var loops: Bool { get set }
  @available(OSX 10.8, *)
  var pointOfView: SCNNode? { get set }
  var autoenablesDefaultLighting: Bool { get set }
  var isJitteringEnabled: Bool { get set }
  @available(OSX 10.9, *)
  @discardableResult
  func prepare(_ object: AnyObject, shouldAbortBlock block: (() -> Bool)? = nil) -> Bool
  @available(OSX 10.10, *)
  func prepare(_ objects: [AnyObject], withCompletionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.9, *)
  var showsStatistics: Bool { get set }
  @available(OSX 10.11, *)
  var debugOptions: SCNDebugOptions { get set }
  @available(OSX 10.11, *)
  var renderingAPI: SCNRenderingAPI { get }
  var context: UnsafeMutablePointer<Void>? { get }
  @available(OSX 10.11, *)
  var currentRenderCommandEncoder: MTLRenderCommandEncoder? { get }
  @available(OSX 10.11, *)
  var device: MTLDevice? { get }
  @available(OSX 10.11, *)
  var colorPixelFormat: MTLPixelFormat { get }
  @available(OSX 10.11, *)
  var depthPixelFormat: MTLPixelFormat { get }
  @available(OSX 10.11, *)
  var stencilPixelFormat: MTLPixelFormat { get }
  @available(OSX 10.11, *)
  var commandQueue: MTLCommandQueue? { get }
  @available(OSX 10.11, *)
  var audioListener: SCNNode? { get set }
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var currentTime: NSTimeInterval { get set }
}
protocol SCNSceneRendererDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func renderer(_ renderer: SCNSceneRenderer, updateAtTime time: NSTimeInterval)
  @available(OSX 10.10, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didApplyAnimationsAtTime time: NSTimeInterval)
  @available(OSX 10.10, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didSimulatePhysicsAtTime time: NSTimeInterval)
  @available(OSX 10.8, *)
  optional func renderer(_ renderer: SCNSceneRenderer, willRenderScene scene: SCNScene, atTime time: NSTimeInterval)
  @available(OSX 10.8, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didRenderScene scene: SCNScene, atTime time: NSTimeInterval)
}

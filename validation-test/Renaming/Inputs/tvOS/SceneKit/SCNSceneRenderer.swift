
let SCNHitTestFirstFoundOnlyKey: String
let SCNHitTestSortResultsKey: String
let SCNHitTestClipToZRangeKey: String
let SCNHitTestBackFaceCullingKey: String
let SCNHitTestBoundingBoxOnlyKey: String
let SCNHitTestIgnoreChildNodesKey: String
let SCNHitTestRootNodeKey: String
@available(tvOS 8.0, *)
let SCNHitTestIgnoreHiddenNodesKey: String
@available(tvOS 9.0, *)
enum SCNRenderingAPI : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case metal
  case openGLES2
}
@available(tvOS 9.0, *)
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
@available(tvOS 8.0, *)
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
  @available(tvOS 8.0, *)
  var scene: SCNScene? { get set }
  @available(tvOS 8.0, *)
  var sceneTime: NSTimeInterval { get set }
  unowned(unsafe) var delegate: @sil_unmanaged SCNSceneRendererDelegate? { get set }
  @available(tvOS 8.0, *)
  @discardableResult
  func hitTest(_ point: CGPoint, options options: [String : AnyObject]? = [:]) -> [SCNHitTestResult]
  @available(tvOS 8.0, *)
  @discardableResult
  func isNode(insideFrustum node: SCNNode, withPointOfView pointOfView: SCNNode) -> Bool
  @available(tvOS 9.0, *)
  @discardableResult
  func nodesInsideFrustum(withPointOfView pointOfView: SCNNode) -> [SCNNode]
  @available(tvOS 8.0, *)
  @discardableResult
  func projectPoint(_ point: SCNVector3) -> SCNVector3
  @available(tvOS 8.0, *)
  @discardableResult
  func unprojectPoint(_ point: SCNVector3) -> SCNVector3
  var isPlaying: Bool { get set }
  var loops: Bool { get set }
  @available(tvOS 8.0, *)
  var pointOfView: SCNNode? { get set }
  var autoenablesDefaultLighting: Bool { get set }
  var isJitteringEnabled: Bool { get set }
  @available(tvOS 8.0, *)
  @discardableResult
  func prepare(_ object: AnyObject, shouldAbortBlock block: (() -> Bool)? = nil) -> Bool
  @available(tvOS 8.0, *)
  func prepare(_ objects: [AnyObject], withCompletionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(tvOS 8.0, *)
  var showsStatistics: Bool { get set }
  @available(tvOS 9.0, *)
  var debugOptions: SCNDebugOptions { get set }
  @available(tvOS 9.0, *)
  var renderingAPI: SCNRenderingAPI { get }
  var context: UnsafeMutablePointer<Void>? { get }
  @available(tvOS 9.0, *)
  var currentRenderCommandEncoder: MTLRenderCommandEncoder? { get }
  @available(tvOS 9.0, *)
  var device: MTLDevice? { get }
  @available(tvOS 9.0, *)
  var colorPixelFormat: MTLPixelFormat { get }
  @available(tvOS 9.0, *)
  var depthPixelFormat: MTLPixelFormat { get }
  @available(tvOS 9.0, *)
  var stencilPixelFormat: MTLPixelFormat { get }
  @available(tvOS 9.0, *)
  var commandQueue: MTLCommandQueue? { get }
  @available(tvOS 9.0, *)
  var audioListener: SCNNode? { get set }
}
protocol SCNSceneRendererDelegate : NSObjectProtocol {
  @available(tvOS 8.0, *)
  optional func renderer(_ renderer: SCNSceneRenderer, updateAtTime time: NSTimeInterval)
  @available(tvOS 8.0, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didApplyAnimationsAtTime time: NSTimeInterval)
  @available(tvOS 8.0, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didSimulatePhysicsAtTime time: NSTimeInterval)
  @available(tvOS 8.0, *)
  optional func renderer(_ renderer: SCNSceneRenderer, willRenderScene scene: SCNScene, atTime time: NSTimeInterval)
  @available(tvOS 8.0, *)
  optional func renderer(_ renderer: SCNSceneRenderer, didRenderScene scene: SCNScene, atTime time: NSTimeInterval)
}

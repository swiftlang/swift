
@available(tvOS 8.0, *)
enum SCNAntialiasingMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case multisampling2X
  case multisampling4X
}
@available(tvOS 9.0, *)
let SCNPreferredRenderingAPIKey: String
@available(tvOS 9.0, *)
let SCNPreferredDeviceKey: String
@available(tvOS 9.0, *)
let SCNPreferLowPowerDeviceKey: String
class SCNView : UIView, SCNSceneRenderer, SCNTechniqueSupport {
  init(frame frame: CGRect, options options: [String : AnyObject]? = [:])
  var allowsCameraControl: Bool
  @available(tvOS 8.0, *)
  @discardableResult
  func snapshot() -> UIImage
  @IBAction func play(_ sender: AnyObject?)
  @IBAction func pause(_ sender: AnyObject?)
  @IBAction func stop(_ sender: AnyObject?)
  var preferredFramesPerSecond: Int
  var eaglContext: EAGLContext?
  @available(tvOS 8.0, *)
  var antialiasingMode: SCNAntialiasingMode
}

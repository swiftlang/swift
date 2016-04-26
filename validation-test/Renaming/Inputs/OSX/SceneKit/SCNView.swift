
@available(OSX 10.10, *)
enum SCNAntialiasingMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case multisampling2X
  case multisampling4X
  case multisampling8X
  case multisampling16X
}
@available(OSX 10.11, *)
let SCNPreferredRenderingAPIKey: String
@available(OSX 10.11, *)
let SCNPreferredDeviceKey: String
@available(OSX 10.11, *)
let SCNPreferLowPowerDeviceKey: String
@available(OSX 10.8, *)
class SCNView : NSView, SCNSceneRenderer, SCNTechniqueSupport {
  init(frame frame: NSRect, options options: [String : AnyObject]? = [:])
  @NSCopying var backgroundColor: NSColor
  var allowsCameraControl: Bool
  @available(OSX 10.10, *)
  @discardableResult
  func snapshot() -> NSImage
  @IBAction func play(_ sender: AnyObject?)
  @IBAction func pause(_ sender: AnyObject?)
  @IBAction func stop(_ sender: AnyObject?)
  var openGLContext: NSOpenGLContext?
  @available(OSX 10.10, *)
  var antialiasingMode: SCNAntialiasingMode
  var pixelFormat: NSOpenGLPixelFormat?
}

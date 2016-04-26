
extension NSObject {
  class func webPlug(inContainerLoad request: NSURLRequest!, inFrame target: String!)
  func webPlug(inContainerLoad request: NSURLRequest!, inFrame target: String!)
  class func webPlug(inContainerShowStatus message: String!)
  func webPlug(inContainerShowStatus message: String!)
  var webPlugInContainerSelectionColor: NSColor! { get }
  var webFrame: WebFrame! { get }
  class func webPlugInContainerSelectionColor() -> NSColor!
  class func webFrame() -> WebFrame!
}

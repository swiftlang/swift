
@available(OSX 10.5, *)
class SBApplication : SBObject, NSCoding {
  init?(bundleIdentifier ident: String)
  init?(url url: NSURL)
  init?(processIdentifier pid: pid_t)
  @discardableResult
  func classForScriptingClass(_ className: String) -> AnyClass?
  var isRunning: Bool { get }
  func activate()
  var delegate: SBApplicationDelegate?
  var launchFlags: LSLaunchFlags
  var sendMode: AESendMode
  var timeout: Int
}
protocol SBApplicationDelegate {
  @discardableResult
  func eventDidFail(_ event: UnsafePointer<AppleEvent>, withError error: NSError) -> AnyObject
}

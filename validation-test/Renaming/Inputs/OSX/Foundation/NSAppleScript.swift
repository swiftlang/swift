
@available(OSX 10.2, *)
let NSAppleScriptErrorMessage: String
@available(OSX 10.2, *)
let NSAppleScriptErrorNumber: String
@available(OSX 10.2, *)
let NSAppleScriptErrorAppName: String
@available(OSX 10.2, *)
let NSAppleScriptErrorBriefMessage: String
@available(OSX 10.2, *)
let NSAppleScriptErrorRange: String
class NSAppleScript : NSObject, NSCopying {
  init?(contentsOf url: NSURL, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(source source: String)
  var source: String? { get }
  var isCompiled: Bool { get }
  @discardableResult
  func compileAndReturnError(_ errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
  @discardableResult
  func executeAndReturnError(_ errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor
  @discardableResult
  func executeAppleEvent(_ event: NSAppleEventDescriptor, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor
}


@available(OSX 10.8, *)
class NSUserScriptTask : NSObject {
  init(url url: NSURL) throws
  @NSCopying var scriptURL: NSURL { get }
  func execute(completionHandler handler: NSUserScriptTaskCompletionHandler? = nil)
}
typealias NSUserScriptTaskCompletionHandler = (NSError?) -> Void
@available(OSX 10.8, *)
class NSUserUnixTask : NSUserScriptTask {
  var standardInput: NSFileHandle?
  var standardOutput: NSFileHandle?
  var standardError: NSFileHandle?
  func execute(withArguments arguments: [String]?, completionHandler handler: NSUserUnixTaskCompletionHandler? = nil)
}
typealias NSUserUnixTaskCompletionHandler = (NSError?) -> Void
@available(OSX 10.8, *)
class NSUserAppleScriptTask : NSUserScriptTask {
  func execute(withAppleEvent event: NSAppleEventDescriptor?, completionHandler handler: NSUserAppleScriptTaskCompletionHandler? = nil)
}
typealias NSUserAppleScriptTaskCompletionHandler = (NSAppleEventDescriptor?, NSError?) -> Void
@available(OSX 10.8, *)
class NSUserAutomatorTask : NSUserScriptTask {
  var variables: [String : AnyObject]?
  func execute(withInput input: NSSecureCoding?, completionHandler handler: NSUserAutomatorTaskCompletionHandler? = nil)
}
typealias NSUserAutomatorTaskCompletionHandler = (AnyObject?, NSError?) -> Void

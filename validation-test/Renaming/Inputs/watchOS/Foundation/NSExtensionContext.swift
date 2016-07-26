
@available(watchOS 2.0, *)
class NSExtensionContext : NSObject {
  var inputItems: [AnyObject] { get }
  func completeRequest(returningItems items: [AnyObject]?, completionHandler completionHandler: ((Bool) -> Void)? = nil)
  func cancelRequest(withError error: NSError)
  func open(_ URL: NSURL, completionHandler completionHandler: ((Bool) -> Void)? = nil)
}
@available(watchOS 2.0, *)
let NSExtensionItemsAndErrorsKey: String
@available(watchOS 2.0, *)
let NSExtensionHostWillEnterForegroundNotification: String
@available(watchOS 2.0, *)
let NSExtensionHostDidEnterBackgroundNotification: String
@available(watchOS 2.0, *)
let NSExtensionHostWillResignActiveNotification: String
@available(watchOS 2.0, *)
let NSExtensionHostDidBecomeActiveNotification: String


@available(watchOS 2.0, *)
class WKExtension : NSObject {
  @discardableResult
  class func shared() -> WKExtension
  func openSystemURL(_ url: NSURL)
  weak var delegate: @sil_weak WKExtensionDelegate?
  var rootInterfaceController: WKInterfaceController? { get }
}
@available(watchOS 2.0, *)
protocol WKExtensionDelegate : NSObjectProtocol {
  optional func applicationDidFinishLaunching()
  optional func applicationDidBecomeActive()
  optional func applicationWillResignActive()
  optional func handleAction(withIdentifier identifier: String?, forRemoteNotification remoteNotification: [NSObject : AnyObject])
  optional func handleAction(withIdentifier identifier: String?, for localNotification: UILocalNotification)
  optional func handleAction(withIdentifier identifier: String?, forRemoteNotification remoteNotification: [NSObject : AnyObject], withResponseInfo responseInfo: [NSObject : AnyObject])
  optional func handleAction(withIdentifier identifier: String?, for localNotification: UILocalNotification, withResponseInfo responseInfo: [NSObject : AnyObject])
  optional func handleUserActivity(_ userInfo: [NSObject : AnyObject]?)
  optional func didReceiveRemoteNotification(_ userInfo: [NSObject : AnyObject])
  optional func didReceive(_ notification: UILocalNotification)
}

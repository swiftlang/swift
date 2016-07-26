
@available(iOS 9.3, *)
enum WCSessionActivationState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notActivated
  case inactive
  case activated
}
@available(iOS 9.0, *)
class WCSession : NSObject {
  @discardableResult
  class func isSupported() -> Bool
  @discardableResult
  class func defaultSession() -> WCSession
  weak var delegate: @sil_weak WCSessionDelegate?
  func activate()
  @available(iOS 9.3, *)
  var activationState: WCSessionActivationState { get }
  var isPaired: Bool { get }
  var isWatchAppInstalled: Bool { get }
  var isComplicationEnabled: Bool { get }
  var watchDirectoryURL: NSURL? { get }
  var isReachable: Bool { get }
  func sendMessage(_ message: [String : AnyObject], replyHandler replyHandler: (([String : AnyObject]) -> Void)?, errorHandler errorHandler: ((NSError) -> Void)? = nil)
  func sendMessageData(_ data: NSData, replyHandler replyHandler: ((NSData) -> Void)?, errorHandler errorHandler: ((NSError) -> Void)? = nil)
  var applicationContext: [String : AnyObject] { get }
  func updateApplicationContext(_ applicationContext: [String : AnyObject]) throws
  var receivedApplicationContext: [String : AnyObject] { get }
  @discardableResult
  func transferUserInfo(_ userInfo: [String : AnyObject] = [:]) -> WCSessionUserInfoTransfer
  @discardableResult
  func transferCurrentComplicationUserInfo(_ userInfo: [String : AnyObject] = [:]) -> WCSessionUserInfoTransfer
  var outstandingUserInfoTransfers: [WCSessionUserInfoTransfer] { get }
  @discardableResult
  func transferFile(_ file: NSURL, metadata metadata: [String : AnyObject]?) -> WCSessionFileTransfer
  var outstandingFileTransfers: [WCSessionFileTransfer] { get }
}
protocol WCSessionDelegate : NSObjectProtocol {
  @available(iOS 9.3, *)
  optional func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error error: NSError?)
  @available(iOS 9.3, *)
  optional func sessionDidBecomeInactive(_ session: WCSession)
  @available(iOS 9.3, *)
  optional func sessionDidDeactivate(_ session: WCSession)
  @available(iOS 9.0, *)
  optional func sessionWatchStateDidChange(_ session: WCSession)
  @available(iOS 9.0, *)
  optional func sessionReachabilityDidChange(_ session: WCSession)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveMessage message: [String : AnyObject])
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveMessage message: [String : AnyObject], replyHandler replyHandler: ([String : AnyObject]) -> Void)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveMessageData messageData: NSData)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveMessageData messageData: NSData, replyHandler replyHandler: (NSData) -> Void)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveApplicationContext applicationContext: [String : AnyObject])
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didFinish userInfoTransfer: WCSessionUserInfoTransfer, error error: NSError?)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceiveUserInfo userInfo: [String : AnyObject] = [:])
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didFinish fileTransfer: WCSessionFileTransfer, error error: NSError?)
  @available(iOS 9.0, *)
  optional func session(_ session: WCSession, didReceive file: WCSessionFile)
}

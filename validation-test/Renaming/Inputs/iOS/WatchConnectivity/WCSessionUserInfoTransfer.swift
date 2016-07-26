
@available(iOS 9.0, *)
class WCSessionUserInfoTransfer : NSObject, NSSecureCoding {
  var isCurrentComplicationInfo: Bool { get }
  var userInfo: [String : AnyObject] { get }
  var isTransferring: Bool { get }
  func cancel()
}


@available(watchOS 2.0, *)
class WCSessionUserInfoTransfer : NSObject, NSSecureCoding {
  var userInfo: [String : AnyObject] { get }
  var isTransferring: Bool { get }
  func cancel()
}

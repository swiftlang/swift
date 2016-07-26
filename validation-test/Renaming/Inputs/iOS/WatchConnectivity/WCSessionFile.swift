
@available(iOS 9.0, *)
class WCSessionFile : NSObject {
  var fileURL: NSURL { get }
  var metadata: [String : AnyObject]? { get }
}
@available(iOS 9.0, *)
class WCSessionFileTransfer : NSObject {
  var file: WCSessionFile { get }
  var isTransferring: Bool { get }
  func cancel()
}

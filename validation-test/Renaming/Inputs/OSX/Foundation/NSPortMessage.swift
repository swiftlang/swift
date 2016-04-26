
class NSPortMessage : NSObject {
  init(send sendPort: NSPort?, receive replyPort: NSPort?, components components: [AnyObject]?)
  var components: [AnyObject]? { get }
  var receivePort: NSPort? { get }
  var sendPort: NSPort? { get }
  @discardableResult
  func send(before date: NSDate) -> Bool
  var msgid: UInt32
}

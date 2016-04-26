
@available(iOS 3.0, *)
class EASession : NSObject {
  @available(iOS 3.0, *)
  init(accessory accessory: EAAccessory, forProtocol protocolString: String)
  @available(iOS 3.0, *)
  var accessory: EAAccessory { get }
  @available(iOS 3.0, *)
  var protocolString: String { get }
  @available(iOS 3.0, *)
  var inputStream: NSInputStream? { get }
  @available(iOS 3.0, *)
  var outputStream: NSOutputStream? { get }
}

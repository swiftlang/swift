
@available(OSX 10.11, *)
class PHAdjustmentData : NSObject {
  init(formatIdentifier formatIdentifier: String, formatVersion formatVersion: String, data data: NSData)
  var formatIdentifier: String { get }
  var formatVersion: String { get }
  var data: NSData { get }
}

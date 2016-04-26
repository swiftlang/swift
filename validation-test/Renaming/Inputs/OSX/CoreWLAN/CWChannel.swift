
@available(OSX 10.7, *)
class CWChannel : NSObject, NSCopying, NSSecureCoding {
  @available(OSX 10.7, *)
  var channelNumber: Int { get }
  @available(OSX 10.7, *)
  var channelWidth: CWChannelWidth { get }
  @available(OSX 10.7, *)
  var channelBand: CWChannelBand { get }
  @available(OSX 10.7, *)
  @discardableResult
  func isEqual(to channel: CWChannel) -> Bool
}

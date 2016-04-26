
enum NSStreamStatus : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notOpen
  case opening
  case open
  case reading
  case writing
  case atEnd
  case closed
  case error
}
struct NSStreamEvent : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var openCompleted: NSStreamEvent { get }
  static var hasBytesAvailable: NSStreamEvent { get }
  static var hasSpaceAvailable: NSStreamEvent { get }
  static var errorOccurred: NSStreamEvent { get }
  static var endEncountered: NSStreamEvent { get }
}
class NSStream : NSObject {
  func open()
  func close()
  unowned(unsafe) var delegate: @sil_unmanaged NSStreamDelegate?
  @discardableResult
  func property(forKey key: String) -> AnyObject?
  @discardableResult
  func setProperty(_ property: AnyObject?, forKey key: String) -> Bool
  func schedule(in aRunLoop: NSRunLoop, forMode mode: String)
  func remove(from aRunLoop: NSRunLoop, forMode mode: String)
  var streamStatus: NSStreamStatus { get }
  @NSCopying var streamError: NSError? { get }
}
class NSInputStream : NSStream {
  @discardableResult
  func read(_ buffer: UnsafeMutablePointer<UInt8>, maxLength len: Int) -> Int
  @discardableResult
  func getBuffer(_ buffer: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>, length len: UnsafeMutablePointer<Int>) -> Bool
  var hasBytesAvailable: Bool { get }
  init(data data: NSData)
  @available(watchOS 2.0, *)
  init?(url url: NSURL)
}
class NSOutputStream : NSStream {
  @discardableResult
  func write(_ buffer: UnsafePointer<UInt8>, maxLength len: Int) -> Int
  var hasSpaceAvailable: Bool { get }
  init(toMemory toMemory: ())
  init(toBuffer buffer: UnsafeMutablePointer<UInt8>, capacity capacity: Int)
  @available(watchOS 2.0, *)
  init?(url url: NSURL, append shouldAppend: Bool)
}
extension NSStream {
}
extension NSStream {
  @available(watchOS 2.0, *)
  class func getBoundStreams(withBufferSize bufferSize: Int, inputStream inputStream: AutoreleasingUnsafeMutablePointer<NSInputStream?>?, outputStream outputStream: AutoreleasingUnsafeMutablePointer<NSOutputStream?>?)
}
extension NSInputStream {
  convenience init?(fileAtPath path: String)
}
extension NSOutputStream {
  convenience init?(toFileAtPath path: String, append shouldAppend: Bool)
  @discardableResult
  class func toMemory() -> Self
}
protocol NSStreamDelegate : NSObjectProtocol {
  optional func stream(_ aStream: NSStream, handle eventCode: NSStreamEvent)
}
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelKey: String
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelNone: String
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelSSLv2: String
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelSSLv3: String
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelTLSv1: String
@available(watchOS 2.0, *)
let NSStreamSocketSecurityLevelNegotiatedSSL: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyConfigurationKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyHostKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyPortKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyVersionKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyUserKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyPasswordKey: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyVersion4: String
@available(watchOS 2.0, *)
let NSStreamSOCKSProxyVersion5: String
@available(watchOS 2.0, *)
let NSStreamDataWrittenToMemoryStreamKey: String
@available(watchOS 2.0, *)
let NSStreamFileCurrentOffsetKey: String
@available(watchOS 2.0, *)
let NSStreamSocketSSLErrorDomain: String
@available(watchOS 2.0, *)
let NSStreamSOCKSErrorDomain: String
@available(watchOS 2.0, *)
let NSStreamNetworkServiceType: String
@available(watchOS 2.0, *)
let NSStreamNetworkServiceTypeVoIP: String
@available(watchOS 2.0, *)
let NSStreamNetworkServiceTypeVideo: String
@available(watchOS 2.0, *)
let NSStreamNetworkServiceTypeBackground: String
@available(watchOS 2.0, *)
let NSStreamNetworkServiceTypeVoice: String

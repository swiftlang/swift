
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
  @available(OSX 10.6, *)
  init?(url url: NSURL)
}
class NSOutputStream : NSStream {
  @discardableResult
  func write(_ buffer: UnsafePointer<UInt8>, maxLength len: Int) -> Int
  var hasSpaceAvailable: Bool { get }
  init(toMemory toMemory: ())
  init(toBuffer buffer: UnsafeMutablePointer<UInt8>, capacity capacity: Int)
  @available(OSX 10.6, *)
  init?(url url: NSURL, append shouldAppend: Bool)
}
extension NSStream {
  @available(OSX 10.10, *)
  class func getStreamsToHost(withName hostname: String, port port: Int, inputStream inputStream: AutoreleasingUnsafeMutablePointer<NSInputStream?>?, outputStream outputStream: AutoreleasingUnsafeMutablePointer<NSOutputStream?>?)
  @available(OSX, introduced: 10.3, deprecated: 10.10, message: "Please use getStreamsToHostWithName:port:inputStream:outputStream: instead")
  class func getStreamsTo(_ host: NSHost, port port: Int, inputStream inputStream: AutoreleasingUnsafeMutablePointer<NSInputStream?>?, outputStream outputStream: AutoreleasingUnsafeMutablePointer<NSOutputStream?>?)
}
extension NSStream {
  @available(OSX 10.10, *)
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
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelKey: String
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelNone: String
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelSSLv2: String
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelSSLv3: String
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelTLSv1: String
@available(OSX 10.3, *)
let NSStreamSocketSecurityLevelNegotiatedSSL: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyConfigurationKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyHostKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyPortKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyVersionKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyUserKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyPasswordKey: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyVersion4: String
@available(OSX 10.3, *)
let NSStreamSOCKSProxyVersion5: String
@available(OSX 10.3, *)
let NSStreamDataWrittenToMemoryStreamKey: String
@available(OSX 10.3, *)
let NSStreamFileCurrentOffsetKey: String
@available(OSX 10.3, *)
let NSStreamSocketSSLErrorDomain: String
@available(OSX 10.3, *)
let NSStreamSOCKSErrorDomain: String
@available(OSX 10.7, *)
let NSStreamNetworkServiceType: String
@available(OSX 10.7, *)
let NSStreamNetworkServiceTypeVoIP: String
@available(OSX 10.7, *)
let NSStreamNetworkServiceTypeVideo: String
@available(OSX 10.7, *)
let NSStreamNetworkServiceTypeBackground: String
@available(OSX 10.7, *)
let NSStreamNetworkServiceTypeVoice: String

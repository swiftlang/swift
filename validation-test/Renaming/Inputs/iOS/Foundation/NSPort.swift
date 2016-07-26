
typealias NSSocketNativeHandle = Int32
let NSPortDidBecomeInvalidNotification: String
class NSPort : NSObject, NSCopying, NSCoding {
  func invalidate()
  var isValid: Bool { get }
  func setDelegate(_ anObject: NSPortDelegate?)
  @discardableResult
  func delegate() -> NSPortDelegate?
  func schedule(in runLoop: NSRunLoop, forMode mode: String)
  func remove(from runLoop: NSRunLoop, forMode mode: String)
  var reservedSpaceLength: Int { get }
  @discardableResult
  func send(before limitDate: NSDate, components components: NSMutableArray?, from receivePort: NSPort?, reserved headerSpaceReserved: Int) -> Bool
  @discardableResult
  func send(before limitDate: NSDate, msgid msgID: Int, components components: NSMutableArray?, from receivePort: NSPort?, reserved headerSpaceReserved: Int) -> Bool
}
protocol NSPortDelegate : NSObjectProtocol {
}
class NSMachPort : NSPort {
  @discardableResult
  class func port(withMachPort machPort: UInt32) -> NSPort
  init(machPort machPort: UInt32)
  @available(iOS 2.0, *)
  @discardableResult
  class func port(withMachPort machPort: UInt32, options f: NSMachPortOptions = []) -> NSPort
  @available(iOS 2.0, *)
  init(machPort machPort: UInt32, options f: NSMachPortOptions = [])
  var machPort: UInt32 { get }
}
@available(iOS 2.0, *)
struct NSMachPortOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var deallocateSendRight: NSMachPortOptions { get }
  static var deallocateReceiveRight: NSMachPortOptions { get }
}
protocol NSMachPortDelegate : NSPortDelegate {
  optional func handleMachMessage(_ msg: UnsafeMutablePointer<Void>)
}
class NSMessagePort : NSPort {
}

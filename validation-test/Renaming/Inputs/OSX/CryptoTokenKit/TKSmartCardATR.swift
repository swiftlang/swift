
@available(OSX 10.10, *)
struct TKSmartCardProtocol : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var T0: TKSmartCardProtocol { get }
  static var T1: TKSmartCardProtocol { get }
  static var T15: TKSmartCardProtocol { get }
  static var any: TKSmartCardProtocol { get }
}
@available(OSX 10.10, *)
class TKSmartCardATRInterfaceGroup : NSObject {
  var ta: NSNumber? { get }
  var tb: NSNumber? { get }
  var tc: NSNumber? { get }
  var `protocol`: NSNumber? { get }
}
@available(OSX 10.10, *)
class TKSmartCardATR : NSObject {
  init?(bytes bytes: NSData)
  init?(source source: () -> Int32)
  var bytes: NSData { get }
  var protocols: [NSNumber] { get }
  @discardableResult
  func interfaceGroup(at index: Int) -> TKSmartCardATRInterfaceGroup?
  @discardableResult
  func interfaceGroup(for protocol: TKSmartCardProtocol) -> TKSmartCardATRInterfaceGroup?
  var historicalBytes: NSData { get }
}

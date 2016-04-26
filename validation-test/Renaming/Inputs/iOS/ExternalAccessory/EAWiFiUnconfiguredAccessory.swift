
struct EAWiFiUnconfiguredAccessoryProperties : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var propertySupportsAirPlay: EAWiFiUnconfiguredAccessoryProperties { get }
  static var propertySupportsAirPrint: EAWiFiUnconfiguredAccessoryProperties { get }
  static var propertySupportsHomeKit: EAWiFiUnconfiguredAccessoryProperties { get }
}
@available(iOS 8.0, *)
class EAWiFiUnconfiguredAccessory : NSObject {
  var name: String { get }
  var manufacturer: String { get }
  var model: String { get }
  var ssid: String { get }
  var macAddress: String { get }
  var properties: EAWiFiUnconfiguredAccessoryProperties { get }
}

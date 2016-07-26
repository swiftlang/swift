
protocol CWEventDelegate {
  optional func clientConnectionInterrupted()
  optional func clientConnectionInvalidated()
  optional func powerStateDidChangeForWiFiInterface(withName interfaceName: String)
  optional func ssidDidChangeForWiFiInterface(withName interfaceName: String)
  optional func bssidDidChangeForWiFiInterface(withName interfaceName: String)
  optional func countryCodeDidChangeForWiFiInterface(withName interfaceName: String)
  optional func linkDidChangeForWiFiInterface(withName interfaceName: String)
  optional func linkQualityDidChangeForWiFiInterface(withName interfaceName: String, rssi rssi: Int, transmitRate transmitRate: Double)
  optional func modeDidChangeForWiFiInterface(withName interfaceName: String)
  optional func scanCacheUpdatedForWiFiInterface(withName interfaceName: String)
}
@available(OSX 10.10, *)
class CWWiFiClient : NSObject {
  @available(OSX 10.10, *)
  weak var delegate: @sil_weak AnyObject?
  @available(OSX 10.10, *)
  @discardableResult
  class func shared() -> CWWiFiClient
  @available(OSX 10.10, *)
  @discardableResult
  func interface() -> CWInterface?
  @available(OSX 10.10, *)
  @discardableResult
  class func interfaceNames() -> [String]?
  @available(OSX 10.10, *)
  @discardableResult
  func interface(withName interfaceName: String?) -> CWInterface?
  @available(OSX 10.10, *)
  @discardableResult
  func interfaces() -> [CWInterface]?
  @available(OSX 10.10, *)
  func startMonitoringEvent(with type: CWEventType) throws
  @available(OSX 10.10, *)
  func stopMonitoringEvent(with type: CWEventType) throws
  @available(OSX 10.10, *)
  func stopMonitoringAllEvents() throws
}

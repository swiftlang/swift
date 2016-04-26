
@available(OSX 10.6, *)
class CWNetwork : NSObject, NSCopying, NSSecureCoding {
  @available(OSX 10.6, *)
  var ssid: String? { get }
  @available(OSX 10.7, *)
  var ssidData: NSData? { get }
  @available(OSX 10.6, *)
  var bssid: String? { get }
  @available(OSX 10.7, *)
  var wlanChannel: CWChannel { get }
  @available(OSX 10.7, *)
  var rssiValue: Int { get }
  @available(OSX 10.7, *)
  var noiseMeasurement: Int { get }
  @available(OSX 10.7, *)
  var informationElementData: NSData? { get }
  @available(OSX 10.7, *)
  var countryCode: String? { get }
  @available(OSX 10.7, *)
  var beaconInterval: Int { get }
  @available(OSX 10.7, *)
  var ibss: Bool { get }
  @available(OSX 10.6, *)
  @discardableResult
  func isEqual(to network: CWNetwork) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func supportsSecurity(_ security: CWSecurity) -> Bool
  @available(OSX 10.8, *)
  @discardableResult
  func supportsPHYMode(_ phyMode: CWPHYMode) -> Bool
}


@available(OSX 10.7, *)
class CWNetworkProfile : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  @available(OSX 10.7, *)
  var ssid: String? { get }
  @available(OSX 10.7, *)
  @NSCopying var ssidData: NSData? { get }
  @available(OSX 10.7, *)
  var security: CWSecurity { get }
  @available(OSX 10.7, *)
  init(networkProfile networkProfile: CWNetworkProfile)
  @available(OSX 10.7, *)
  @discardableResult
  func isEqual(to networkProfile: CWNetworkProfile) -> Bool
}
@available(OSX 10.7, *)
class CWMutableNetworkProfile : CWNetworkProfile {
}

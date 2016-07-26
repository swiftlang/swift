
@available(OSX 10.8, *)
class AVBInterface : NSObject {
  var interfaceName: String { get }
  var entityDiscovery: AVB17221EntityDiscovery? { get }
  var aecp: AVB17221AECPInterface? { get }
  var acmp: AVB17221ACMPInterface? { get }
  @discardableResult
  class func macAddress(forInterfaceNamed anInterfaceName: String) -> AVBMACAddress?
  @discardableResult
  class func supportedInterfaces() -> [String]?
  @discardableResult
  class func isAVBEnabled(onInterfaceNamed anInterfaceName: String) -> Bool
  @discardableResult
  class func isAVBCapableInterfaceNamed(_ anInterfaceName: String) -> Bool
  init?(interfaceName anInterfaceName: String)
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  class func myGUID() -> UInt64
  @discardableResult
  class func myEntityID() -> UInt64
}

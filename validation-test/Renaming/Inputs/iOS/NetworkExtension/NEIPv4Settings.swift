
@available(iOS 9.0, *)
class NEIPv4Settings : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(addresses addresses: [String], subnetMasks subnetMasks: [String])
  @available(iOS 9.0, *)
  var addresses: [String] { get }
  @available(iOS 9.0, *)
  var subnetMasks: [String] { get }
  @available(iOS 9.0, *)
  var includedRoutes: [NEIPv4Route]?
  @available(iOS 9.0, *)
  var excludedRoutes: [NEIPv4Route]?
}
@available(iOS 9.0, *)
class NEIPv4Route : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(destinationAddress address: String, subnetMask subnetMask: String)
  @available(iOS 9.0, *)
  var destinationAddress: String { get }
  @available(iOS 9.0, *)
  var destinationSubnetMask: String { get }
  @available(iOS 9.0, *)
  var gatewayAddress: String?
  @available(iOS 9.0, *)
  @discardableResult
  class func defaultRoute() -> NEIPv4Route
}

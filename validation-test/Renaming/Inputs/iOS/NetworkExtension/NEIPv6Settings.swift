
@available(iOS 9.0, *)
class NEIPv6Settings : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(addresses addresses: [String], networkPrefixLengths networkPrefixLengths: [NSNumber])
  @available(iOS 9.0, *)
  var addresses: [String] { get }
  @available(iOS 9.0, *)
  var networkPrefixLengths: [NSNumber] { get }
  @available(iOS 9.0, *)
  var includedRoutes: [NEIPv6Route]?
  @available(iOS 9.0, *)
  var excludedRoutes: [NEIPv6Route]?
}
@available(iOS 9.0, *)
class NEIPv6Route : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(destinationAddress address: String, networkPrefixLength networkPrefixLength: NSNumber)
  @available(iOS 9.0, *)
  var destinationAddress: String { get }
  @available(iOS 9.0, *)
  var destinationNetworkPrefixLength: NSNumber { get }
  @available(iOS 9.0, *)
  var gatewayAddress: String?
  @available(iOS 9.0, *)
  @discardableResult
  class func defaultRoute() -> NEIPv6Route
}

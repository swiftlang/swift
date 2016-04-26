
@available(iOS 9.0, *)
class NEDNSSettings : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(servers servers: [String])
  @available(iOS 9.0, *)
  var servers: [String] { get }
  @available(iOS 9.0, *)
  var searchDomains: [String]?
  @available(iOS 9.0, *)
  var domainName: String?
  @available(iOS 9.0, *)
  var matchDomains: [String]?
  @available(iOS 9.0, *)
  var matchDomainsNoSearch: Bool
}

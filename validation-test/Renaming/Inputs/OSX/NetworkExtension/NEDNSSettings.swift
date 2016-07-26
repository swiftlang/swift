
@available(OSX 10.11, *)
class NEDNSSettings : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.11, *)
  init(servers servers: [String])
  @available(OSX 10.11, *)
  var servers: [String] { get }
  @available(OSX 10.11, *)
  var searchDomains: [String]?
  @available(OSX 10.11, *)
  var domainName: String?
  @available(OSX 10.11, *)
  var matchDomains: [String]?
  @available(OSX 10.11, *)
  var matchDomainsNoSearch: Bool
}

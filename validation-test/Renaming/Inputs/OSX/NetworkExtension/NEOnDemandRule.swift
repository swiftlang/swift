
@available(OSX 10.10, *)
enum NEOnDemandRuleAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case connect
  case disconnect
  case evaluateConnection
  case ignore
}
@available(OSX 10.10, *)
enum NEOnDemandRuleInterfaceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX 10.11, *)
  case any
  @available(OSX 10.10, *)
  case ethernet
  @available(OSX 10.10, *)
  case wiFi
}
@available(OSX 10.10, *)
class NEOnDemandRule : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.10, *)
  var action: NEOnDemandRuleAction { get }
  @available(OSX 10.10, *)
  var dnsSearchDomainMatch: [String]?
  @available(OSX 10.10, *)
  var dnsServerAddressMatch: [String]?
  @available(OSX 10.10, *)
  var interfaceTypeMatch: NEOnDemandRuleInterfaceType
  @available(OSX 10.10, *)
  var ssidMatch: [String]?
  @available(OSX 10.10, *)
  @NSCopying var probeURL: NSURL?
}
@available(OSX 10.10, *)
class NEOnDemandRuleConnect : NEOnDemandRule {
}
@available(OSX 10.10, *)
class NEOnDemandRuleDisconnect : NEOnDemandRule {
}
@available(OSX 10.10, *)
class NEOnDemandRuleIgnore : NEOnDemandRule {
}
@available(OSX 10.10, *)
class NEOnDemandRuleEvaluateConnection : NEOnDemandRule {
  @available(OSX 10.10, *)
  var connectionRules: [NEEvaluateConnectionRule]?
}
@available(OSX 10.10, *)
enum NEEvaluateConnectionRuleAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case connectIfNeeded
  case neverConnect
}
@available(OSX 10.10, *)
class NEEvaluateConnectionRule : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.10, *)
  init(matchDomains domains: [String], andAction action: NEEvaluateConnectionRuleAction)
  @available(OSX 10.10, *)
  var action: NEEvaluateConnectionRuleAction { get }
  @available(OSX 10.10, *)
  var matchDomains: [String] { get }
  @available(OSX 10.10, *)
  var useDNSServers: [String]?
  @available(OSX 10.10, *)
  @NSCopying var probeURL: NSURL?
}

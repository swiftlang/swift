
@available(iOS 9.0, *)
class NWBonjourServiceEndpoint : NWEndpoint {
  @available(iOS 9.0, *)
  convenience init(name name: String, type type: String, domain domain: String)
  @available(iOS 9.0, *)
  var name: String { get }
  @available(iOS 9.0, *)
  var type: String { get }
  @available(iOS 9.0, *)
  var domain: String { get }
}

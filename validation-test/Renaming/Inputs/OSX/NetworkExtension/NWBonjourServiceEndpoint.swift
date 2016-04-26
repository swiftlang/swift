
@available(OSX 10.11, *)
class NWBonjourServiceEndpoint : NWEndpoint {
  @available(OSX 10.11, *)
  convenience init(name name: String, type type: String, domain domain: String)
  @available(OSX 10.11, *)
  var name: String { get }
  @available(OSX 10.11, *)
  var type: String { get }
  @available(OSX 10.11, *)
  var domain: String { get }
}

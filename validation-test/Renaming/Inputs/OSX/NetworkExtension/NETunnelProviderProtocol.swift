
@available(OSX 10.11, *)
class NETunnelProviderProtocol : NEVPNProtocol {
  @available(OSX 10.11, *)
  var providerConfiguration: [String : AnyObject]?
  @available(OSX 10.11, *)
  var providerBundleIdentifier: String?
}

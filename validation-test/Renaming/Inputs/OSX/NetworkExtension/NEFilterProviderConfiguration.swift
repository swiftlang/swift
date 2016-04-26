
@available(OSX 10.11, *)
class NEFilterProviderConfiguration : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.11, *)
  var filterBrowsers: Bool
  @available(OSX 10.11, *)
  var filterSockets: Bool
  @available(OSX 10.11, *)
  var vendorConfiguration: [String : AnyObject]?
  @available(OSX 10.11, *)
  var serverAddress: String?
  @available(OSX 10.11, *)
  var username: String?
  @available(OSX 10.11, *)
  var organization: String?
  @available(OSX 10.11, *)
  @NSCopying var passwordReference: NSData?
  @available(OSX 10.11, *)
  @NSCopying var identityReference: NSData?
}

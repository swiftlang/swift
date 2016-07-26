
@available(OSX 10.6, *)
class CWConfiguration : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  @available(OSX 10.7, *)
  @NSCopying var networkProfiles: NSOrderedSet { get }
  @available(OSX 10.7, *)
  var requireAdministratorForAssociation: Bool { get }
  @available(OSX 10.7, *)
  var requireAdministratorForPower: Bool { get }
  @available(OSX 10.7, *)
  var requireAdministratorForIBSSMode: Bool { get }
  @available(OSX 10.7, *)
  var rememberJoinedNetworks: Bool { get }
  @available(OSX 10.7, *)
  init(configuration configuration: CWConfiguration)
  @available(OSX 10.6, *)
  @discardableResult
  func isEqual(to configuration: CWConfiguration) -> Bool
}
@available(OSX 10.6, *)
class CWMutableConfiguration : CWConfiguration {
}

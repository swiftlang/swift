
class ODRecord : NSObject {
  @available(OSX 10.6, *)
  func setNodeCredentials(_ inUsername: String!, password inPassword: String!) throws
  @available(OSX 10.6, *)
  func setNodeCredentialsWithRecordType(_ inRecordType: String!, authenticationType inType: String!, authenticationItems inItems: [AnyObject]!, continueItems outItems: AutoreleasingUnsafeMutablePointer<NSArray?>!, context outContext: AutoreleasingUnsafeMutablePointer<AnyObject?>!) throws
  @available(OSX 10.6, *)
  func verifyPassword(_ inPassword: String!) throws
  @available(OSX 10.6, *)
  func verifyExtended(withAuthenticationType inType: String!, authenticationItems inItems: [AnyObject]!, continueItems outItems: AutoreleasingUnsafeMutablePointer<NSArray?>!, context outContext: AutoreleasingUnsafeMutablePointer<AnyObject?>!) throws
  @available(OSX 10.6, *)
  func changePassword(_ oldPassword: String!, toPassword newPassword: String!) throws
  @available(OSX 10.6, *)
  func synchronize() throws
  @available(OSX 10.6, *)
  var recordType: String! { get }
  @available(OSX 10.6, *)
  var recordName: String! { get }
  @available(OSX 10.6, *)
  @discardableResult
  func recordDetails(forAttributes inAttributes: [AnyObject]!) throws -> [NSObject : AnyObject]
  @available(OSX 10.6, *)
  @discardableResult
  func values(forAttribute inAttribute: String!) throws -> [AnyObject]
  @available(OSX 10.6, *)
  func setValue(_ inValueOrValues: AnyObject!, forAttribute inAttribute: String!) throws
  @available(OSX 10.6, *)
  func removeValues(forAttribute inAttribute: String!) throws
  @available(OSX 10.6, *)
  func addValue(_ inValue: AnyObject!, toAttribute inAttribute: String!) throws
  @available(OSX 10.6, *)
  func removeValue(_ inValue: AnyObject!, fromAttribute inAttribute: String!) throws
  @available(OSX 10.6, *)
  func delete() throws
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use accountPoliciesAndReturnError:")
  @discardableResult
  func policies() throws -> [NSObject : AnyObject]
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use authenticationAllowedAndReturnError: and similar methods")
  @discardableResult
  func effectivePolicies() throws -> [NSObject : AnyObject]
  @available(OSX, introduced: 10.9, deprecated: 10.10)
  @discardableResult
  func supportedPolicies() throws -> [NSObject : AnyObject]
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use setAccountPolicies:error:")
  func setPolicies(_ policies: [NSObject : AnyObject]!) throws
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use addAccountPolicy:toCategory:error:")
  func setPolicy(_ policy: ODPolicyType!, value value: AnyObject!) throws
  @available(OSX, introduced: 10.9, deprecated: 10.10, message: "use removeAccountPolicy:fromCategory:error:")
  func remove(_ policy: ODPolicyType!) throws
  @available(OSX 10.10, *)
  func addAccountPolicy(_ policy: [NSObject : AnyObject]!, toCategory category: String!) throws
  @available(OSX 10.10, *)
  func removeAccountPolicy(_ policy: [NSObject : AnyObject]!, fromCategory category: String!) throws
  @available(OSX 10.10, *)
  func setAccountPolicies(_ policies: [NSObject : AnyObject]!) throws
  @available(OSX 10.10, *)
  @discardableResult
  func accountPolicies() throws -> [NSObject : AnyObject]
  @available(OSX 10.10, *)
  func authenticationAllowed() throws
  @available(OSX 10.10, *)
  func passwordChangeAllowed(_ newPassword: String!) throws
  @available(OSX 10.10, *)
  @discardableResult
  func willPasswordExpire(_ willExpireIn: UInt64) -> Bool
  @available(OSX 10.10, *)
  @discardableResult
  func willAuthenticationsExpire(_ willExpireIn: UInt64) -> Bool
  @available(OSX 10.10, *)
  var secondsUntilPasswordExpires: Int64 { get }
  @available(OSX 10.10, *)
  var secondsUntilAuthenticationsExpire: Int64 { get }
}
extension ODRecord {
  @available(OSX 10.6, *)
  func addMemberRecord(_ inRecord: ODRecord!) throws
  @available(OSX 10.6, *)
  func removeMemberRecord(_ inRecord: ODRecord!) throws
  @available(OSX 10.6, *)
  func isMemberRecord(_ inRecord: ODRecord!) throws
}

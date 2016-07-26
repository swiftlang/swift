
@available(OSX 10.10, *)
class CKDiscoveredUserInfo : NSObject {
  @NSCopying var userRecordID: CKRecordID? { get }
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use -[[CKDiscoveredUserInfo displayContact] givenName]")
  var firstName: String? { get }
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use -[[CKDiscoveredUserInfo displayContact] familyName]")
  var lastName: String? { get }
  @available(OSX 10.11, *)
  @NSCopying var displayContact: CNContact? { get }
}

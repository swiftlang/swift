
@available(iOS 8.0, *)
class CKDiscoveredUserInfo : NSObject {
  @NSCopying var userRecordID: CKRecordID? { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use -[[CKDiscoveredUserInfo displayContact] givenName]")
  var firstName: String? { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Use -[[CKDiscoveredUserInfo displayContact] familyName]")
  var lastName: String? { get }
  @available(iOS 9.0, *)
  @NSCopying var displayContact: CNContact? { get }
}

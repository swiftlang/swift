
@available(watchOS 2.0, *)
enum CNEntityType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case contacts
}
@available(watchOS 2.0, *)
enum CNAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case restricted
  case denied
  case authorized
}
@available(watchOS 2.0, *)
class CNContactStore : NSObject {
  @discardableResult
  class func authorizationStatus(for entityType: CNEntityType) -> CNAuthorizationStatus
  func requestAccess(for entityType: CNEntityType, completionHandler completionHandler: (Bool, NSError?) -> Void)
  @discardableResult
  func unifiedContacts(matching predicate: NSPredicate, keysToFetch keys: [CNKeyDescriptor]) throws -> [CNContact]
  @discardableResult
  func unifiedContact(withIdentifier identifier: String, keysToFetch keys: [CNKeyDescriptor]) throws -> CNContact
  func enumerateContacts(with fetchRequest: CNContactFetchRequest, usingBlock block: (CNContact, UnsafeMutablePointer<ObjCBool>) -> Void) throws
  @discardableResult
  func groups(matching predicate: NSPredicate?) throws -> [CNGroup]
  @discardableResult
  func containers(matching predicate: NSPredicate?) throws -> [CNContainer]
  @discardableResult
  func defaultContainerIdentifier() -> String
}
@available(watchOS 2.0, *)
let CNContactStoreDidChangeNotification: String

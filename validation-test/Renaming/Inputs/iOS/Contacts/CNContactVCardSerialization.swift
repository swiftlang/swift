
@available(iOS 9.0, *)
class CNContactVCardSerialization : NSObject {
  @discardableResult
  class func descriptorForRequiredKeys() -> CNKeyDescriptor
  @discardableResult
  class func data(withContacts contacts: [AnyObject]) throws -> NSData
  @discardableResult
  class func contacts(with data: NSData) throws -> [AnyObject]
}

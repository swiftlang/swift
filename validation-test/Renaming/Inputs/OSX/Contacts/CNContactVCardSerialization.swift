
@available(OSX 10.11, *)
class CNContactVCardSerialization : NSObject {
  @discardableResult
  class func descriptorForRequiredKeys() -> CNKeyDescriptor
  @discardableResult
  class func data(withContacts contacts: [AnyObject]) throws -> NSData
  @discardableResult
  class func contacts(with data: NSData) throws -> [AnyObject]
}

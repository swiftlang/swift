
extension CNContact {
  @discardableResult
  class func predicateForContacts(matchingName name: String) -> NSPredicate
  @discardableResult
  class func predicateForContacts(withIdentifiers identifiers: [String]) -> NSPredicate
  @discardableResult
  class func predicateForContactsInGroup(withIdentifier groupIdentifier: String) -> NSPredicate
  @discardableResult
  class func predicateForContactsInContainer(withIdentifier containerIdentifier: String) -> NSPredicate
}

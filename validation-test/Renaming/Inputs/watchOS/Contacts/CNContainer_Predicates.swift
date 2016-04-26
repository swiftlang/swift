
extension CNContainer {
  @discardableResult
  class func predicateForContainers(withIdentifiers identifiers: [String]) -> NSPredicate
  @discardableResult
  class func predicateForContainerOfContact(withIdentifier contactIdentifier: String) -> NSPredicate
  @discardableResult
  class func predicateForContainerOfGroup(withIdentifier groupIdentifier: String) -> NSPredicate
}

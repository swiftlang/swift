
extension CNGroup {
  @discardableResult
  class func predicateForGroups(withIdentifiers identifiers: [String]) -> NSPredicate
  @discardableResult
  class func predicateForGroupsInContainer(withIdentifier containerIdentifier: String) -> NSPredicate
}

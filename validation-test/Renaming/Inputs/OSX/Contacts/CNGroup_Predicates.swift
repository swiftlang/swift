
extension CNGroup {
  @discardableResult
  class func predicateForGroups(withIdentifiers identifiers: [String]) -> NSPredicate
  @available(OSX 10.11, *)
  @discardableResult
  class func predicateForSubgroupsInGroup(withIdentifier parentGroupIdentifier: String) -> NSPredicate
  @discardableResult
  class func predicateForGroupsInContainer(withIdentifier containerIdentifier: String) -> NSPredicate
}

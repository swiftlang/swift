
class ABGroup : ABRecord {
  @discardableResult
  func members() -> [AnyObject]!
  @discardableResult
  func addMember(_ person: ABPerson!) -> Bool
  @discardableResult
  func removeMember(_ person: ABPerson!) -> Bool
  @discardableResult
  func subgroups() -> [AnyObject]!
  @discardableResult
  func addSubgroup(_ group: ABGroup!) -> Bool
  @discardableResult
  func removeSubgroup(_ group: ABGroup!) -> Bool
  @discardableResult
  func parentGroups() -> [AnyObject]!
  @discardableResult
  func setDistributionIdentifier(_ identifier: String!, forProperty property: String!, person person: ABPerson!) -> Bool
  @discardableResult
  func distributionIdentifier(forProperty property: String!, person person: ABPerson!) -> String!
}
extension ABGroup {
  @discardableResult
  class func addPropertiesAndTypes(_ properties: [NSObject : AnyObject]!) -> Int
  @discardableResult
  class func removeProperties(_ properties: [AnyObject]!) -> Int
  @discardableResult
  class func properties() -> [AnyObject]!
  @discardableResult
  class func type(ofProperty property: String!) -> ABPropertyType
}
extension ABGroup {
  @discardableResult
  class func searchElement(forProperty property: String!, label label: String!, key key: String!, value value: AnyObject!, comparison comparison: ABSearchComparison) -> ABSearchElement!
}

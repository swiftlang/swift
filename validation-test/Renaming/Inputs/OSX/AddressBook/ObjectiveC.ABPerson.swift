
class ABPerson : ABRecord {
  @discardableResult
  func parentGroups() -> [AnyObject]!
  @available(OSX 10.8, *)
  @discardableResult
  func linkedPeople() -> [AnyObject]!
}
extension ABPerson {
  @discardableResult
  class func addPropertiesAndTypes(_ properties: [NSObject : AnyObject]!) -> Int
  @discardableResult
  class func removeProperties(_ properties: [AnyObject]!) -> Int
  @discardableResult
  class func properties() -> [AnyObject]!
  @discardableResult
  class func type(ofProperty property: String!) -> ABPropertyType
}
extension ABPerson {
  @discardableResult
  class func searchElement(forProperty property: String!, label label: String!, key key: String!, value value: AnyObject!, comparison comparison: ABSearchComparison) -> ABSearchElement!
}
extension ABPerson {
  init!(vCardRepresentation vCardData: NSData!)
  @discardableResult
  func vCardRepresentation() -> NSData!
}

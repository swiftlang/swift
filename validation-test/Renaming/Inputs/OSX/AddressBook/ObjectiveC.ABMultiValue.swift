
class ABMultiValue : NSObject, NSCopying, NSMutableCopying, NSFastEnumeration {
  @discardableResult
  func count() -> Int
  @discardableResult
  func value(at index: Int) -> AnyObject!
  @discardableResult
  func label(at index: Int) -> String!
  @discardableResult
  func identifier(at index: Int) -> String!
  @discardableResult
  func index(forIdentifier identifier: String!) -> Int
  @discardableResult
  func primaryIdentifier() -> String!
  @discardableResult
  func propertyType() -> ABPropertyType
  @discardableResult
  func value(forIdentifier identifier: String!) -> AnyObject!
  @discardableResult
  func label(forIdentifier identifier: String!) -> AnyObject!
}
class ABMutableMultiValue : ABMultiValue {
  @discardableResult
  func add(_ value: AnyObject!, withLabel label: String!) -> String!
  @discardableResult
  func insert(_ value: AnyObject!, withLabel label: String!, at index: Int) -> String!
  @discardableResult
  func removeAndLabel(at index: Int) -> Bool
  @discardableResult
  func replace(at index: Int, withValue value: AnyObject!) -> Bool
  @discardableResult
  func replaceLabel(at index: Int, withLabel label: String!) -> Bool
  @discardableResult
  func setPrimaryIdentifier(_ identifier: String!) -> Bool
}

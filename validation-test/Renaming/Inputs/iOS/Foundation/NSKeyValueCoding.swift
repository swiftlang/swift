
let NSUndefinedKeyException: String
let NSAverageKeyValueOperator: String
let NSCountKeyValueOperator: String
let NSDistinctUnionOfArraysKeyValueOperator: String
let NSDistinctUnionOfObjectsKeyValueOperator: String
let NSDistinctUnionOfSetsKeyValueOperator: String
let NSMaximumKeyValueOperator: String
let NSMinimumKeyValueOperator: String
let NSSumKeyValueOperator: String
let NSUnionOfArraysKeyValueOperator: String
let NSUnionOfObjectsKeyValueOperator: String
let NSUnionOfSetsKeyValueOperator: String
extension NSObject {
  @discardableResult
  class func accessInstanceVariablesDirectly() -> Bool
  @discardableResult
  class func value(forKey key: String) -> AnyObject?
  @discardableResult
  func value(forKey key: String) -> AnyObject?
  class func setValue(_ value: AnyObject?, forKey key: String)
  func setValue(_ value: AnyObject?, forKey key: String)
  class func validateValue(_ ioValue: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKey inKey: String) throws
  func validateValue(_ ioValue: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKey inKey: String) throws
  @discardableResult
  class func mutableArrayValue(forKey key: String) -> NSMutableArray
  @discardableResult
  func mutableArrayValue(forKey key: String) -> NSMutableArray
  @available(iOS 5.0, *)
  @discardableResult
  class func mutableOrderedSetValue(forKey key: String) -> NSMutableOrderedSet
  @available(iOS 5.0, *)
  @discardableResult
  func mutableOrderedSetValue(forKey key: String) -> NSMutableOrderedSet
  @discardableResult
  class func mutableSetValue(forKey key: String) -> NSMutableSet
  @discardableResult
  func mutableSetValue(forKey key: String) -> NSMutableSet
  @discardableResult
  class func value(forKeyPath keyPath: String) -> AnyObject?
  @discardableResult
  func value(forKeyPath keyPath: String) -> AnyObject?
  class func setValue(_ value: AnyObject?, forKeyPath keyPath: String)
  func setValue(_ value: AnyObject?, forKeyPath keyPath: String)
  class func validateValue(_ ioValue: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKeyPath inKeyPath: String) throws
  func validateValue(_ ioValue: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKeyPath inKeyPath: String) throws
  @discardableResult
  class func mutableArrayValue(forKeyPath keyPath: String) -> NSMutableArray
  @discardableResult
  func mutableArrayValue(forKeyPath keyPath: String) -> NSMutableArray
  @available(iOS 5.0, *)
  @discardableResult
  class func mutableOrderedSetValue(forKeyPath keyPath: String) -> NSMutableOrderedSet
  @available(iOS 5.0, *)
  @discardableResult
  func mutableOrderedSetValue(forKeyPath keyPath: String) -> NSMutableOrderedSet
  @discardableResult
  class func mutableSetValue(forKeyPath keyPath: String) -> NSMutableSet
  @discardableResult
  func mutableSetValue(forKeyPath keyPath: String) -> NSMutableSet
  @discardableResult
  class func value(forUndefinedKey key: String) -> AnyObject?
  @discardableResult
  func value(forUndefinedKey key: String) -> AnyObject?
  class func setValue(_ value: AnyObject?, forUndefinedKey key: String)
  func setValue(_ value: AnyObject?, forUndefinedKey key: String)
  class func setNilValueForKey(_ key: String)
  func setNilValueForKey(_ key: String)
  @discardableResult
  class func dictionaryWithValues(forKeys keys: [String]) -> [String : AnyObject]
  @discardableResult
  func dictionaryWithValues(forKeys keys: [String]) -> [String : AnyObject]
  class func setValuesForKeys(_ keyedValues: [String : AnyObject])
  func setValuesForKeys(_ keyedValues: [String : AnyObject])
}
extension NSArray {
}
extension NSDictionary {
}
extension NSMutableDictionary {
}
extension NSOrderedSet {
}
extension NSSet {
}


class NSDictionary : NSObject, NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration {
  var count: Int { get }
  @discardableResult
  func object(forKey aKey: AnyObject) -> AnyObject?
  @discardableResult
  func keyEnumerator() -> NSEnumerator
  init(objects objects: UnsafePointer<AnyObject>!, forKeys keys: UnsafePointer<NSCopying>!, count cnt: Int)
}

extension NSDictionary : DictionaryLiteralConvertible {
}

extension NSDictionary : Sequence {
}

extension NSDictionary {
  @objc(_swiftInitWithDictionary_NSDictionary:) convenience init(dictionary otherDictionary: NSDictionary)
}

extension NSDictionary : CustomReflectable {
}
extension NSDictionary {
  var allKeys: [AnyObject] { get }
  @discardableResult
  func allKeys(for anObject: AnyObject) -> [AnyObject]
  var allValues: [AnyObject] { get }
  var descriptionInStringsFileFormat: String { get }
  @discardableResult
  func description(withLocale locale: AnyObject?) -> String
  @discardableResult
  func description(withLocale locale: AnyObject?, indent level: Int) -> String
  @discardableResult
  func isEqual(to otherDictionary: [NSObject : AnyObject]) -> Bool
  @discardableResult
  func objectEnumerator() -> NSEnumerator
  @discardableResult
  func objects(forKeys keys: [AnyObject], notFoundMarker marker: AnyObject) -> [AnyObject]
  @discardableResult
  func write(toFile path: String, atomically useAuxiliaryFile: Bool) -> Bool
  @discardableResult
  func write(to url: NSURL, atomically atomically: Bool) -> Bool
  @discardableResult
  func keysSortedByValue(using comparator: Selector) -> [AnyObject]
  @available(watchOS 2.0, *)
  func getObjects(_ objects: AutoreleasingUnsafeMutablePointer<AnyObject>!, andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject>!, count count: Int)
  @available(watchOS 2.0, *)
  subscript(_ key: NSCopying) -> AnyObject? { get }
  @available(watchOS 2.0, *)
  func enumerateKeysAndObjects(_ block: (AnyObject, AnyObject, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(watchOS 2.0, *)
  func enumerateKeysAndObjects(_ opts: NSEnumerationOptions = [], using block: (AnyObject, AnyObject, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(watchOS 2.0, *)
  @discardableResult
  func keysSortedByValue(comparator cmptr: NSComparator) -> [AnyObject]
  @available(watchOS 2.0, *)
  @discardableResult
  func keysSortedByValue(_ opts: NSSortOptions = [], usingComparator cmptr: NSComparator) -> [AnyObject]
  @available(watchOS 2.0, *)
  @discardableResult
  func keysOfEntries(passingTest predicate: (AnyObject, AnyObject, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Set<NSObject>
  @available(watchOS 2.0, *)
  @discardableResult
  func keysOfEntries(_ opts: NSEnumerationOptions = [], passingTest predicate: (AnyObject, AnyObject, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Set<NSObject>
}
extension NSDictionary {
  func getObjects(_ objects: AutoreleasingUnsafeMutablePointer<AnyObject>!, andKeys keys: AutoreleasingUnsafeMutablePointer<AnyObject>!)
}
extension NSDictionary {
  convenience init(object object: AnyObject, forKey key: NSCopying)
  convenience init(dictionary otherDictionary: [NSObject : AnyObject])
  convenience init(dictionary otherDictionary: [NSObject : AnyObject], copyItems flag: Bool)
  convenience init(objects objects: [AnyObject], forKeys keys: [NSCopying])
  convenience init?(contentsOfFile path: String)
  convenience init?(contentsOf url: NSURL)
}
class NSMutableDictionary : NSDictionary {
  func removeObject(forKey aKey: AnyObject)
  func setObject(_ anObject: AnyObject, forKey aKey: NSCopying)
  init(capacity numItems: Int)
}
extension NSMutableDictionary {
  func addEntries(from otherDictionary: [NSObject : AnyObject])
  func removeAllObjects()
  func removeObjects(forKeys keyArray: [AnyObject])
  func setDictionary(_ otherDictionary: [NSObject : AnyObject])
}
extension NSMutableDictionary {
}
extension NSDictionary {
  @available(watchOS 2.0, *)
  @discardableResult
  class func sharedKeySet(forKeys keys: [NSCopying]) -> AnyObject
}
extension NSMutableDictionary {
  @available(watchOS 2.0, *)
  /*not inherited*/ init(sharedKeySet keyset: AnyObject)
}

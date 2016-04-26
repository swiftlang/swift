
class NSSet : NSObject, NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration {
  var count: Int { get }
  @discardableResult
  func member(_ object: AnyObject) -> AnyObject?
  @discardableResult
  func objectEnumerator() -> NSEnumerator
  init(objects objects: UnsafePointer<AnyObject>!, count cnt: Int)
}

extension NSSet : Sequence {
}

extension NSSet {
  convenience init(objects elements: AnyObject...)
}

extension NSSet : ArrayLiteralConvertible {
}

extension NSSet {
  @objc(_swiftInitWithSet_NSSet:) convenience init(set anSet: NSSet)
}

extension NSSet : CustomReflectable {
}
extension NSSet {
  var allObjects: [AnyObject] { get }
  @discardableResult
  func anyObject() -> AnyObject?
  @discardableResult
  func contains(_ anObject: AnyObject) -> Bool
  @discardableResult
  func description(withLocale locale: AnyObject?) -> String
  @discardableResult
  func intersects(_ otherSet: Set<NSObject>) -> Bool
  @discardableResult
  func isEqual(to otherSet: Set<NSObject>) -> Bool
  @discardableResult
  func isSubset(of otherSet: Set<NSObject>) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func adding(_ anObject: AnyObject) -> Set<NSObject>
  @available(tvOS 2.0, *)
  @discardableResult
  func addingObjects(from other: Set<NSObject>) -> Set<NSObject>
  @available(tvOS 2.0, *)
  @discardableResult
  func addingObjects(from other: [AnyObject]) -> Set<NSObject>
  @available(tvOS 4.0, *)
  func enumerateObjects(_ block: (AnyObject, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 4.0, *)
  func enumerateObjects(_ opts: NSEnumerationOptions = [], using block: (AnyObject, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 4.0, *)
  @discardableResult
  func objects(passingTest predicate: (AnyObject, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Set<NSObject>
  @available(tvOS 4.0, *)
  @discardableResult
  func objects(_ opts: NSEnumerationOptions = [], passingTest predicate: (AnyObject, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Set<NSObject>
}
extension NSSet {
  convenience init(object object: AnyObject)
  convenience init(set set: Set<NSObject>)
  convenience init(set set: Set<NSObject>, copyItems flag: Bool)
  convenience init(array array: [AnyObject])
}
class NSMutableSet : NSSet {
  func add(_ object: AnyObject)
  func remove(_ object: AnyObject)
  init(capacity numItems: Int)
}
extension NSMutableSet {
  func addObjects(from array: [AnyObject])
  func intersect(_ otherSet: Set<NSObject>)
  func minus(_ otherSet: Set<NSObject>)
  func removeAllObjects()
  func union(_ otherSet: Set<NSObject>)
  func setSet(_ otherSet: Set<NSObject>)
}
extension NSMutableSet {
}
class NSCountedSet : NSMutableSet {
  @discardableResult
  func count(for object: AnyObject) -> Int
}

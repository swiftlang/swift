
@available(watchOS 2.0, *)
class NSOrderedSet : NSObject, NSCopying, NSMutableCopying, NSSecureCoding, NSFastEnumeration {
  var count: Int { get }
  @discardableResult
  func object(at idx: Int) -> AnyObject
  @discardableResult
  func index(of object: AnyObject) -> Int
  init(objects objects: UnsafePointer<AnyObject>!, count cnt: Int)
}

extension NSOrderedSet : Sequence {
}

extension NSOrderedSet {
  convenience init(objects elements: AnyObject...)
}

extension NSOrderedSet : ArrayLiteralConvertible {
}
extension NSOrderedSet {
  func getObjects(_ objects: AutoreleasingUnsafeMutablePointer<AnyObject>!, range range: NSRange)
  @discardableResult
  func objects(at indexes: NSIndexSet) -> [AnyObject]
  var firstObject: AnyObject? { get }
  var lastObject: AnyObject? { get }
  @discardableResult
  func isEqual(to other: NSOrderedSet) -> Bool
  @discardableResult
  func contains(_ object: AnyObject) -> Bool
  @discardableResult
  func intersects(_ other: NSOrderedSet) -> Bool
  @discardableResult
  func intersectsSet(_ set: Set<NSObject>) -> Bool
  @discardableResult
  func isSubset(of other: NSOrderedSet) -> Bool
  @discardableResult
  func isSubset(of set: Set<NSObject>) -> Bool
  @available(watchOS 2.0, *)
  subscript(_ idx: Int) -> AnyObject { get }
  @discardableResult
  func objectEnumerator() -> NSEnumerator
  @discardableResult
  func reverseObjectEnumerator() -> NSEnumerator
  @NSCopying var reversed: NSOrderedSet { get }
  var array: [AnyObject] { get }
  var set: Set<NSObject> { get }
  func enumerateObjects(_ block: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateObjects(options opts: NSEnumerationOptions = [], using block: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateObjects(at s: NSIndexSet, options opts: NSEnumerationOptions = [], using block: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @discardableResult
  func index(ofObjectPassingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @discardableResult
  func index(_ opts: NSEnumerationOptions = [], ofObjectPassingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @discardableResult
  func index(ofObjectAt s: NSIndexSet, options opts: NSEnumerationOptions = [], passingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @discardableResult
  func indexes(ofObjectsPassingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @discardableResult
  func indexes(options opts: NSEnumerationOptions = [], ofObjectsPassingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @discardableResult
  func indexes(ofObjectsAt s: NSIndexSet, options opts: NSEnumerationOptions = [], passingTest predicate: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @discardableResult
  func index(of object: AnyObject, inSortedRange range: NSRange, options opts: NSBinarySearchingOptions = [], usingComparator cmp: NSComparator) -> Int
  @discardableResult
  func sortedArray(comparator cmptr: NSComparator) -> [AnyObject]
  @discardableResult
  func sortedArray(_ opts: NSSortOptions = [], usingComparator cmptr: NSComparator) -> [AnyObject]
  @discardableResult
  func description(withLocale locale: AnyObject?) -> String
  @discardableResult
  func description(withLocale locale: AnyObject?, indent level: Int) -> String
}
extension NSOrderedSet {
  convenience init(object object: AnyObject)
  convenience init(orderedSet set: NSOrderedSet)
  convenience init(orderedSet set: NSOrderedSet, copyItems flag: Bool)
  convenience init(orderedSet set: NSOrderedSet, range range: NSRange, copyItems flag: Bool)
  convenience init(array array: [AnyObject])
  convenience init(array set: [AnyObject], copyItems flag: Bool)
  convenience init(array set: [AnyObject], range range: NSRange, copyItems flag: Bool)
  convenience init(set set: Set<NSObject>)
  convenience init(set set: Set<NSObject>, copyItems flag: Bool)
}
@available(watchOS 2.0, *)
class NSMutableOrderedSet : NSOrderedSet {
  func insert(_ object: AnyObject, at idx: Int)
  func removeObject(at idx: Int)
  func replaceObject(at idx: Int, with object: AnyObject)
  init(capacity numItems: Int)
}
extension NSMutableOrderedSet {
  func add(_ object: AnyObject)
  func add(_ objects: UnsafePointer<AnyObject>!, count count: Int)
  func addObjects(from array: [AnyObject])
  func exchangeObject(at idx1: Int, withObjectAt idx2: Int)
  func moveObjects(at indexes: NSIndexSet, to idx: Int)
  func insert(_ objects: [AnyObject], at indexes: NSIndexSet)
  func setObject(_ obj: AnyObject, at idx: Int)
  func replaceObjects(in range: NSRange, with objects: UnsafePointer<AnyObject>!, count count: Int)
  func replaceObjects(at indexes: NSIndexSet, with objects: [AnyObject])
  func removeObjects(in range: NSRange)
  func removeObjects(at indexes: NSIndexSet)
  func removeAllObjects()
  func remove(_ object: AnyObject)
  func removeObjects(in array: [AnyObject])
  func intersect(_ other: NSOrderedSet)
  func minus(_ other: NSOrderedSet)
  func union(_ other: NSOrderedSet)
  func intersectSet(_ other: Set<NSObject>)
  func minusSet(_ other: Set<NSObject>)
  func unionSet(_ other: Set<NSObject>)
  func sort(comparator cmptr: NSComparator)
  func sort(_ opts: NSSortOptions = [], usingComparator cmptr: NSComparator)
  func sortRange(_ range: NSRange, options opts: NSSortOptions = [], usingComparator cmptr: NSComparator)
}
extension NSMutableOrderedSet {
}

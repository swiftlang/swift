
var __FOUNDATION_NSHASHTABLE__: Int32 { get }
@available(tvOS 6.0, *)
let NSHashTableStrongMemory: NSPointerFunctionsOptions
@available(tvOS 6.0, *)
let NSHashTableCopyIn: NSPointerFunctionsOptions
@available(tvOS 6.0, *)
let NSHashTableObjectPointerPersonality: NSPointerFunctionsOptions
@available(tvOS 6.0, *)
let NSHashTableWeakMemory: NSPointerFunctionsOptions
typealias NSHashTableOptions = Int
@available(tvOS 6.0, *)
class NSHashTable<ObjectType : AnyObject> : NSObject, NSCopying, NSCoding, NSFastEnumeration {
  init(options options: NSPointerFunctionsOptions = [], capacity initialCapacity: Int)
  init(pointerFunctions functions: NSPointerFunctions, capacity initialCapacity: Int)
  /*not inherited*/ init(options options: NSPointerFunctionsOptions = [])
  @available(tvOS 6.0, *)
  @discardableResult
  class func weakObjects() -> NSHashTable<ObjectType>
  @NSCopying var pointerFunctions: NSPointerFunctions { get }
  var count: Int { get }
  @discardableResult
  func member(_ object: ObjectType?) -> ObjectType?
  @discardableResult
  func objectEnumerator() -> NSEnumerator
  func add(_ object: ObjectType?)
  func remove(_ object: ObjectType?)
  func removeAllObjects()
  var allObjects: [ObjectType] { get }
  var anyObject: ObjectType? { get }
  @discardableResult
  func contains(_ anObject: ObjectType?) -> Bool
  @discardableResult
  func intersects(_ other: NSHashTable<ObjectType>) -> Bool
  @discardableResult
  func isEqual(to other: NSHashTable<ObjectType>) -> Bool
  @discardableResult
  func isSubset(of other: NSHashTable<ObjectType>) -> Bool
  func intersect(_ other: NSHashTable<ObjectType>)
  func union(_ other: NSHashTable<ObjectType>)
  func minus(_ other: NSHashTable<ObjectType>)
  var setRepresentation: Set<NSObject> { get }
  convenience init()
}

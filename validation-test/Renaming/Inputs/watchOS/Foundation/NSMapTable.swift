
var __FOUNDATION_NSMAPTABLE__: Int32 { get }
@available(watchOS 2.0, *)
let NSMapTableStrongMemory: NSPointerFunctionsOptions
@available(watchOS 2.0, *)
let NSMapTableCopyIn: NSPointerFunctionsOptions
@available(watchOS 2.0, *)
let NSMapTableObjectPointerPersonality: NSPointerFunctionsOptions
@available(watchOS 2.0, *)
let NSMapTableWeakMemory: NSPointerFunctionsOptions
typealias NSMapTableOptions = Int
@available(watchOS 2.0, *)
class NSMapTable<KeyType : AnyObject, ObjectType : AnyObject> : NSObject, NSCopying, NSCoding, NSFastEnumeration {
  init(keyOptions keyOptions: NSPointerFunctionsOptions = [], valueOptions valueOptions: NSPointerFunctionsOptions = [], capacity initialCapacity: Int)
  init(keyPointerFunctions keyFunctions: NSPointerFunctions, valuePointerFunctions valueFunctions: NSPointerFunctions, capacity initialCapacity: Int)
  /*not inherited*/ init(keyOptions keyOptions: NSPointerFunctionsOptions = [], valueOptions valueOptions: NSPointerFunctionsOptions = [])
  @available(watchOS 2.0, *)
  @discardableResult
  class func strongToStrongObjects() -> NSMapTable<KeyType, ObjectType>
  @available(watchOS 2.0, *)
  @discardableResult
  class func weakToStrongObjects() -> NSMapTable<KeyType, ObjectType>
  @available(watchOS 2.0, *)
  @discardableResult
  class func strongToWeakObjects() -> NSMapTable<KeyType, ObjectType>
  @available(watchOS 2.0, *)
  @discardableResult
  class func weakToWeakObjects() -> NSMapTable<KeyType, ObjectType>
  @NSCopying var keyPointerFunctions: NSPointerFunctions { get }
  @NSCopying var valuePointerFunctions: NSPointerFunctions { get }
  @discardableResult
  func object(forKey aKey: KeyType?) -> ObjectType?
  func removeObject(forKey aKey: KeyType?)
  func setObject(_ anObject: ObjectType?, forKey aKey: KeyType?)
  var count: Int { get }
  @discardableResult
  func keyEnumerator() -> NSEnumerator
  @discardableResult
  func objectEnumerator() -> NSEnumerator?
  func removeAllObjects()
  @discardableResult
  func dictionaryRepresentation() -> [NSObject : ObjectType]
  convenience init()
}

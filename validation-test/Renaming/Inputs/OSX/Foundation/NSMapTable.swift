
var __FOUNDATION_NSMAPTABLE__: Int32 { get }
@available(OSX 10.5, *)
let NSMapTableStrongMemory: NSPointerFunctionsOptions
@available(OSX 10.5, *)
let NSMapTableCopyIn: NSPointerFunctionsOptions
@available(OSX 10.5, *)
let NSMapTableObjectPointerPersonality: NSPointerFunctionsOptions
@available(OSX 10.8, *)
let NSMapTableWeakMemory: NSPointerFunctionsOptions
typealias NSMapTableOptions = Int
@available(OSX 10.5, *)
class NSMapTable<KeyType : AnyObject, ObjectType : AnyObject> : NSObject, NSCopying, NSCoding, NSFastEnumeration {
  init(keyOptions keyOptions: NSPointerFunctionsOptions = [], valueOptions valueOptions: NSPointerFunctionsOptions = [], capacity initialCapacity: Int)
  init(keyPointerFunctions keyFunctions: NSPointerFunctions, valuePointerFunctions valueFunctions: NSPointerFunctions, capacity initialCapacity: Int)
  /*not inherited*/ init(keyOptions keyOptions: NSPointerFunctionsOptions = [], valueOptions valueOptions: NSPointerFunctionsOptions = [])
  @available(OSX 10.8, *)
  @discardableResult
  class func strongToStrongObjects() -> NSMapTable<KeyType, ObjectType>
  @available(OSX 10.8, *)
  @discardableResult
  class func weakToStrongObjects() -> NSMapTable<KeyType, ObjectType>
  @available(OSX 10.8, *)
  @discardableResult
  class func strongToWeakObjects() -> NSMapTable<KeyType, ObjectType>
  @available(OSX 10.8, *)
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
struct NSMapEnumerator {
  var _pi: Int
  var _si: Int
  var _bs: UnsafeMutablePointer<Void>?
  init()
  init(_pi _pi: Int, _si _si: Int, _bs _bs: UnsafeMutablePointer<Void>?)
}
func NSFreeMapTable(_ table: NSMapTable<AnyObject, AnyObject>)
func NSResetMapTable(_ table: NSMapTable<AnyObject, AnyObject>)
@discardableResult
func NSCompareMapTables(_ table1: NSMapTable<AnyObject, AnyObject>, _ table2: NSMapTable<AnyObject, AnyObject>) -> Bool
@discardableResult
func NSCopyMapTableWithZone(_ table: NSMapTable<AnyObject, AnyObject>, _ zone: NSZone?) -> NSMapTable<AnyObject, AnyObject>
@discardableResult
func NSMapMember(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>, _ originalKey: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?, _ value: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?) -> Bool
@discardableResult
func NSMapGet(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>?) -> UnsafeMutablePointer<Void>?
func NSMapInsert(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>?, _ value: UnsafePointer<Void>?)
func NSMapInsertKnownAbsent(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>?, _ value: UnsafePointer<Void>?)
@discardableResult
func NSMapInsertIfAbsent(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>?, _ value: UnsafePointer<Void>?) -> UnsafeMutablePointer<Void>?
func NSMapRemove(_ table: NSMapTable<AnyObject, AnyObject>, _ key: UnsafePointer<Void>?)
@discardableResult
func NSEnumerateMapTable(_ table: NSMapTable<AnyObject, AnyObject>) -> NSMapEnumerator
@discardableResult
func NSNextMapEnumeratorPair(_ enumerator: UnsafeMutablePointer<NSMapEnumerator>, _ key: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?, _ value: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>?) -> Bool
func NSEndMapTableEnumeration(_ enumerator: UnsafeMutablePointer<NSMapEnumerator>)
@discardableResult
func NSCountMapTable(_ table: NSMapTable<AnyObject, AnyObject>) -> Int
@discardableResult
func NSStringFromMapTable(_ table: NSMapTable<AnyObject, AnyObject>) -> String
@discardableResult
func NSAllMapTableKeys(_ table: NSMapTable<AnyObject, AnyObject>) -> [AnyObject]
@discardableResult
func NSAllMapTableValues(_ table: NSMapTable<AnyObject, AnyObject>) -> [AnyObject]
struct NSMapTableKeyCallBacks {
  var hash: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Int)?
  var isEqual: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>, UnsafePointer<Void>) -> ObjCBool)?
  var retain: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Void)?
  var release: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafeMutablePointer<Void>) -> Void)?
  var describe: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> String?)?
  var notAKeyMarker: UnsafePointer<Void>?
  init()
  init(hash hash: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Int)?, isEqual isEqual: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>, UnsafePointer<Void>) -> ObjCBool)?, retain retain: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Void)?, release release: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafeMutablePointer<Void>) -> Void)?, describe describe: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> String?)?, notAKeyMarker notAKeyMarker: UnsafePointer<Void>?)
}
struct NSMapTableValueCallBacks {
  var retain: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Void)?
  var release: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafeMutablePointer<Void>) -> Void)?
  var describe: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> String?)?
  init()
  init(retain retain: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> Void)?, release release: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafeMutablePointer<Void>) -> Void)?, describe describe: (@convention(c) (NSMapTable<AnyObject, AnyObject>, UnsafePointer<Void>) -> String?)?)
}
@discardableResult
func NSCreateMapTableWithZone(_ keyCallBacks: NSMapTableKeyCallBacks, _ valueCallBacks: NSMapTableValueCallBacks, _ capacity: Int, _ zone: NSZone?) -> NSMapTable<AnyObject, AnyObject>
@discardableResult
func NSCreateMapTable(_ keyCallBacks: NSMapTableKeyCallBacks, _ valueCallBacks: NSMapTableValueCallBacks, _ capacity: Int) -> NSMapTable<AnyObject, AnyObject>
@available(OSX 10.5, *)
let NSIntegerMapKeyCallBacks: NSMapTableKeyCallBacks
let NSNonOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks
let NSNonOwnedPointerOrNullMapKeyCallBacks: NSMapTableKeyCallBacks
let NSNonRetainedObjectMapKeyCallBacks: NSMapTableKeyCallBacks
let NSObjectMapKeyCallBacks: NSMapTableKeyCallBacks
let NSOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks
@available(OSX 10.5, *)
let NSIntegerMapValueCallBacks: NSMapTableValueCallBacks
let NSNonOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks
let NSObjectMapValueCallBacks: NSMapTableValueCallBacks
let NSNonRetainedObjectMapValueCallBacks: NSMapTableValueCallBacks
let NSOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks

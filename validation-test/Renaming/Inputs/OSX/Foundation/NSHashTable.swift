
var __FOUNDATION_NSHASHTABLE__: Int32 { get }
@available(OSX 10.5, *)
let NSHashTableStrongMemory: NSPointerFunctionsOptions
@available(OSX 10.5, *)
let NSHashTableCopyIn: NSPointerFunctionsOptions
@available(OSX 10.5, *)
let NSHashTableObjectPointerPersonality: NSPointerFunctionsOptions
@available(OSX 10.8, *)
let NSHashTableWeakMemory: NSPointerFunctionsOptions
typealias NSHashTableOptions = Int
@available(OSX 10.5, *)
class NSHashTable<ObjectType : AnyObject> : NSObject, NSCopying, NSCoding, NSFastEnumeration {
  init(options options: NSPointerFunctionsOptions = [], capacity initialCapacity: Int)
  init(pointerFunctions functions: NSPointerFunctions, capacity initialCapacity: Int)
  /*not inherited*/ init(options options: NSPointerFunctionsOptions = [])
  @available(OSX 10.8, *)
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
struct NSHashEnumerator {
  var _pi: Int
  var _si: Int
  var _bs: UnsafeMutablePointer<Void>?
  init()
  init(_pi _pi: Int, _si _si: Int, _bs _bs: UnsafeMutablePointer<Void>?)
}
func NSFreeHashTable(_ table: NSHashTable<AnyObject>)
func NSResetHashTable(_ table: NSHashTable<AnyObject>)
@discardableResult
func NSCompareHashTables(_ table1: NSHashTable<AnyObject>, _ table2: NSHashTable<AnyObject>) -> Bool
@discardableResult
func NSCopyHashTableWithZone(_ table: NSHashTable<AnyObject>, _ zone: NSZone?) -> NSHashTable<AnyObject>
@discardableResult
func NSHashGet(_ table: NSHashTable<AnyObject>, _ pointer: UnsafePointer<Void>?) -> UnsafeMutablePointer<Void>
func NSHashInsert(_ table: NSHashTable<AnyObject>, _ pointer: UnsafePointer<Void>?)
func NSHashInsertKnownAbsent(_ table: NSHashTable<AnyObject>, _ pointer: UnsafePointer<Void>?)
@discardableResult
func NSHashInsertIfAbsent(_ table: NSHashTable<AnyObject>, _ pointer: UnsafePointer<Void>?) -> UnsafeMutablePointer<Void>?
func NSHashRemove(_ table: NSHashTable<AnyObject>, _ pointer: UnsafePointer<Void>?)
@discardableResult
func NSEnumerateHashTable(_ table: NSHashTable<AnyObject>) -> NSHashEnumerator
@discardableResult
func NSNextHashEnumeratorItem(_ enumerator: UnsafeMutablePointer<NSHashEnumerator>) -> UnsafeMutablePointer<Void>?
func NSEndHashTableEnumeration(_ enumerator: UnsafeMutablePointer<NSHashEnumerator>)
@discardableResult
func NSCountHashTable(_ table: NSHashTable<AnyObject>) -> Int
@discardableResult
func NSStringFromHashTable(_ table: NSHashTable<AnyObject>) -> String
@discardableResult
func NSAllHashTableObjects(_ table: NSHashTable<AnyObject>) -> [AnyObject]
struct NSHashTableCallBacks {
  var hash: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> Int)?
  var isEqual: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>, UnsafePointer<Void>) -> ObjCBool)?
  var retain: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> Void)?
  var release: (@convention(c) (NSHashTable<AnyObject>, UnsafeMutablePointer<Void>) -> Void)?
  var describe: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> String?)?
  init()
  init(hash hash: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> Int)?, isEqual isEqual: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>, UnsafePointer<Void>) -> ObjCBool)?, retain retain: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> Void)?, release release: (@convention(c) (NSHashTable<AnyObject>, UnsafeMutablePointer<Void>) -> Void)?, describe describe: (@convention(c) (NSHashTable<AnyObject>, UnsafePointer<Void>) -> String?)?)
}
@discardableResult
func NSCreateHashTableWithZone(_ callBacks: NSHashTableCallBacks, _ capacity: Int, _ zone: NSZone?) -> NSHashTable<AnyObject>
@discardableResult
func NSCreateHashTable(_ callBacks: NSHashTableCallBacks, _ capacity: Int) -> NSHashTable<AnyObject>
@available(OSX 10.5, *)
let NSIntegerHashCallBacks: NSHashTableCallBacks
let NSNonOwnedPointerHashCallBacks: NSHashTableCallBacks
let NSNonRetainedObjectHashCallBacks: NSHashTableCallBacks
let NSObjectHashCallBacks: NSHashTableCallBacks
let NSOwnedObjectIdentityHashCallBacks: NSHashTableCallBacks
let NSOwnedPointerHashCallBacks: NSHashTableCallBacks
let NSPointerToStructHashCallBacks: NSHashTableCallBacks

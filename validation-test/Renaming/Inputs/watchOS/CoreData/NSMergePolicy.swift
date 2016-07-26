
@available(watchOS 2.0, *)
var NSErrorMergePolicy: AnyObject
@available(watchOS 2.0, *)
var NSMergeByPropertyStoreTrumpMergePolicy: AnyObject
@available(watchOS 2.0, *)
var NSMergeByPropertyObjectTrumpMergePolicy: AnyObject
@available(watchOS 2.0, *)
var NSOverwriteMergePolicy: AnyObject
@available(watchOS 2.0, *)
var NSRollbackMergePolicy: AnyObject
enum NSMergePolicyType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case errorMergePolicyType
  case mergeByPropertyStoreTrumpMergePolicyType
  case mergeByPropertyObjectTrumpMergePolicyType
  case overwriteMergePolicyType
  case rollbackMergePolicyType
}
@available(watchOS 2.0, *)
class NSMergeConflict : NSObject {
  var sourceObject: NSManagedObject { get }
  var objectSnapshot: [String : AnyObject]? { get }
  var cachedSnapshot: [String : AnyObject]? { get }
  var persistedSnapshot: [String : AnyObject]? { get }
  var newVersionNumber: Int { get }
  var oldVersionNumber: Int { get }
  init(source srcObject: NSManagedObject, newVersion newvers: Int, oldVersion oldvers: Int, cachedSnapshot cachesnap: [String : AnyObject]?, persistedSnapshot persnap: [String : AnyObject]?)
}
@available(watchOS 2.0, *)
class NSConstraintConflict : NSObject {
  var constraint: [String] { get }
  var constraintValues: [String : AnyObject] { get }
  var databaseObject: NSManagedObject? { get }
  var databaseSnapshot: [String : AnyObject]? { get }
  var conflictingObjects: [NSManagedObject] { get }
  var conflictingSnapshots: [[NSObject : AnyObject]] { get }
  init(constraint contraint: [String], databaseObject databaseObject: NSManagedObject?, databaseSnapshot databaseSnapshot: [NSObject : AnyObject]?, conflictingObjects conflictingObjects: [NSManagedObject], conflictingSnapshots conflictingSnapshots: [AnyObject])
}
@available(watchOS 2.0, *)
class NSMergePolicy : NSObject {
  var mergeType: NSMergePolicyType { get }
  init(merge ty: NSMergePolicyType)
  func resolve(mergeConflicts list: [AnyObject]) throws
  @available(watchOS 2.0, *)
  func resolveOptimisticLockingVersionConflicts(_ list: [NSMergeConflict]) throws
  @available(watchOS 2.0, *)
  func resolve(constraintConflicts list: [NSConstraintConflict]) throws
}

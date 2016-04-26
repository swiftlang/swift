
struct NSFetchRequestResultType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var managedObjectIDResultType: NSFetchRequestResultType { get }
  @available(iOS 3.0, *)
  static var dictionaryResultType: NSFetchRequestResultType { get }
  @available(iOS 3.0, *)
  static var countResultType: NSFetchRequestResultType { get }
}
@available(iOS 3.0, *)
class NSFetchRequest : NSPersistentStoreRequest, NSCoding {
  @available(iOS 4.0, *)
  convenience init(entityName entityName: String)
  var entity: NSEntityDescription?
  @available(iOS 4.0, *)
  var entityName: String? { get }
  var predicate: NSPredicate?
  var sortDescriptors: [NSSortDescriptor]?
  var fetchLimit: Int
  @available(iOS 3.0, *)
  var resultType: NSFetchRequestResultType
  @available(iOS 3.0, *)
  var includesSubentities: Bool
  @available(iOS 3.0, *)
  var includesPropertyValues: Bool
  @available(iOS 3.0, *)
  var returnsObjectsAsFaults: Bool
  @available(iOS 3.0, *)
  var relationshipKeyPathsForPrefetching: [String]?
  @available(iOS 3.0, *)
  var includesPendingChanges: Bool
  @available(iOS 3.0, *)
  var returnsDistinctResults: Bool
  @available(iOS 3.0, *)
  var propertiesToFetch: [AnyObject]?
  @available(iOS 3.0, *)
  var fetchOffset: Int
  @available(iOS 3.0, *)
  var fetchBatchSize: Int
  @available(iOS 5.0, *)
  var shouldRefreshRefetchedObjects: Bool
  @available(iOS 5.0, *)
  var propertiesToGroupBy: [AnyObject]?
  @available(iOS 5.0, *)
  var havingPredicate: NSPredicate?
}
struct _fetchRequestFlags {
  var distinctValuesOnly: UInt32
  var includesSubentities: UInt32
  var includesPropertyValues: UInt32
  var resultType: UInt32
  var returnsObjectsAsFaults: UInt32
  var excludePendingChanges: UInt32
  var isInUse: UInt32
  var entityIsName: UInt32
  var refreshesRefetched: UInt32
  var propertiesValidated: UInt32
  var disableCaching: UInt32
  var _RESERVED: UInt32
  init()
  init(distinctValuesOnly distinctValuesOnly: UInt32, includesSubentities includesSubentities: UInt32, includesPropertyValues includesPropertyValues: UInt32, resultType resultType: UInt32, returnsObjectsAsFaults returnsObjectsAsFaults: UInt32, excludePendingChanges excludePendingChanges: UInt32, isInUse isInUse: UInt32, entityIsName entityIsName: UInt32, refreshesRefetched refreshesRefetched: UInt32, propertiesValidated propertiesValidated: UInt32, disableCaching disableCaching: UInt32, _RESERVED _RESERVED: UInt32)
}

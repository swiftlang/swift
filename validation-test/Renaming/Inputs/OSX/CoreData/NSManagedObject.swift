
struct NSSnapshotEventType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var undoInsertion: NSSnapshotEventType { get }
  static var undoDeletion: NSSnapshotEventType { get }
  static var undoUpdate: NSSnapshotEventType { get }
  static var rollback: NSSnapshotEventType { get }
  static var refresh: NSSnapshotEventType { get }
  static var mergePolicy: NSSnapshotEventType { get }
}
@available(OSX 10.4, *)
class NSManagedObject : NSObject {
  @available(OSX 10.6, *)
  @discardableResult
  class func contextShouldIgnoreUnmodeledPropertyChanges() -> Bool
  init(entity entity: NSEntityDescription, insertInto context: NSManagedObjectContext?)
  unowned(unsafe) var managedObjectContext: @sil_unmanaged NSManagedObjectContext? { get }
  var entity: NSEntityDescription { get }
  var objectID: NSManagedObjectID { get }
  var isInserted: Bool { get }
  var isUpdated: Bool { get }
  var isDeleted: Bool { get }
  @available(OSX 10.7, *)
  var hasChanges: Bool { get }
  @available(OSX 10.9, *)
  var hasPersistentChangedValues: Bool { get }
  var isFault: Bool { get }
  @available(OSX 10.5, *)
  @discardableResult
  func hasFault(forRelationshipNamed key: String) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  func objectIDs(forRelationshipNamed key: String) -> [NSManagedObjectID]
  @available(OSX 10.5, *)
  var faultingState: Int { get }
  func willAccessValue(forKey key: String?)
  func didAccessValue(forKey key: String?)
  func awakeFromFetch()
  func awakeFromInsert()
  @available(OSX 10.6, *)
  func awake(fromSnapshotEvents flags: NSSnapshotEventType)
  @available(OSX 10.6, *)
  func prepareForDeletion()
  func willSave()
  func didSave()
  @available(OSX 10.5, *)
  func willTurnIntoFault()
  func didTurnIntoFault()
  @discardableResult
  func primitiveValue(forKey key: String) -> AnyObject?
  func setPrimitiveValue(_ value: AnyObject?, forKey key: String)
  @discardableResult
  func committedValues(forKeys keys: [String]?) -> [String : AnyObject]
  @discardableResult
  func changedValues() -> [String : AnyObject]
  @available(OSX 10.7, *)
  @discardableResult
  func changedValuesForCurrentEvent() -> [String : AnyObject]
  func validateForDelete() throws
  func validateForInsert() throws
  func validateForUpdate() throws
}

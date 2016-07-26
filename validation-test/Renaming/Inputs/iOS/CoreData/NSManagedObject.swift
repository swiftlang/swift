
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
@available(iOS 3.0, *)
class NSManagedObject : NSObject {
  @available(iOS 3.0, *)
  @discardableResult
  class func contextShouldIgnoreUnmodeledPropertyChanges() -> Bool
  init(entity entity: NSEntityDescription, insertInto context: NSManagedObjectContext?)
  unowned(unsafe) var managedObjectContext: @sil_unmanaged NSManagedObjectContext? { get }
  var entity: NSEntityDescription { get }
  var objectID: NSManagedObjectID { get }
  var isInserted: Bool { get }
  var isUpdated: Bool { get }
  var isDeleted: Bool { get }
  @available(iOS 5.0, *)
  var hasChanges: Bool { get }
  @available(iOS 7.0, *)
  var hasPersistentChangedValues: Bool { get }
  var isFault: Bool { get }
  @available(iOS 3.0, *)
  @discardableResult
  func hasFault(forRelationshipNamed key: String) -> Bool
  @available(iOS 8.3, *)
  @discardableResult
  func objectIDs(forRelationshipNamed key: String) -> [NSManagedObjectID]
  @available(iOS 3.0, *)
  var faultingState: Int { get }
  func willAccessValue(forKey key: String?)
  func didAccessValue(forKey key: String?)
  func awakeFromFetch()
  func awakeFromInsert()
  @available(iOS 3.0, *)
  func awake(fromSnapshotEvents flags: NSSnapshotEventType)
  @available(iOS 3.0, *)
  func prepareForDeletion()
  func willSave()
  func didSave()
  @available(iOS 3.0, *)
  func willTurnIntoFault()
  func didTurnIntoFault()
  @discardableResult
  func primitiveValue(forKey key: String) -> AnyObject?
  func setPrimitiveValue(_ value: AnyObject?, forKey key: String)
  @discardableResult
  func committedValues(forKeys keys: [String]?) -> [String : AnyObject]
  @discardableResult
  func changedValues() -> [String : AnyObject]
  @available(iOS 5.0, *)
  @discardableResult
  func changedValuesForCurrentEvent() -> [String : AnyObject]
  func validateForDelete() throws
  func validateForInsert() throws
  func validateForUpdate() throws
}

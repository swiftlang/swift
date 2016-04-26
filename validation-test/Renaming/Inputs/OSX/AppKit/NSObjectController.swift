
class NSObjectController : NSController {
  init(content content: AnyObject?)
  var content: AnyObject?
  var selection: AnyObject { get }
  var selectedObjects: [AnyObject] { get }
  var automaticallyPreparesContent: Bool
  func prepareContent()
  var objectClass: AnyClass!
  @discardableResult
  func newObject() -> AnyObject
  func add(_ object: AnyObject)
  func remove(_ object: AnyObject)
  var isEditable: Bool
  func add(_ sender: AnyObject?)
  var canAdd: Bool { get }
  func remove(_ sender: AnyObject?)
  var canRemove: Bool { get }
  @discardableResult
  func validate(_ item: NSValidatedUserInterfaceItem) -> Bool
}
struct __objectControllerFlags {
  var _editable: UInt32
  var _automaticallyPreparesContent: UInt32
  var _hasLoadedData: UInt32
  var _explicitlyCannotAdd: UInt32
  var _explicitlyCannotRemove: UInt32
  var _isUsingManagedProxy: UInt32
  var _hasFetched: UInt32
  var _batches: UInt32
  var _reservedObjectController: UInt32
  init()
  init(_editable _editable: UInt32, _automaticallyPreparesContent _automaticallyPreparesContent: UInt32, _hasLoadedData _hasLoadedData: UInt32, _explicitlyCannotAdd _explicitlyCannotAdd: UInt32, _explicitlyCannotRemove _explicitlyCannotRemove: UInt32, _isUsingManagedProxy _isUsingManagedProxy: UInt32, _hasFetched _hasFetched: UInt32, _batches _batches: UInt32, _reservedObjectController _reservedObjectController: UInt32)
}
extension NSObjectController {
  var managedObjectContext: NSManagedObjectContext?
  var entityName: String?
  var fetchPredicate: NSPredicate?
  func fetch(with fetchRequest: NSFetchRequest?, merge merge: Bool) throws
  func fetch(_ sender: AnyObject?)
  @available(OSX 10.5, *)
  var usesLazyFetching: Bool
  @available(OSX 10.5, *)
  @discardableResult
  func defaultFetchRequest() -> NSFetchRequest
}

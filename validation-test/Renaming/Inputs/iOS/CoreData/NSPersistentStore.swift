
@available(iOS 3.0, *)
class NSPersistentStore : NSObject {
  @discardableResult
  class func metadataForPersistentStore(with url: NSURL) throws -> [String : AnyObject]
  class func setMetadata(_ metadata: [String : AnyObject]?, forPersistentStoreAt url: NSURL) throws
  @available(iOS 3.0, *)
  @discardableResult
  class func migrationManagerClass() -> AnyClass
  init(persistentStoreCoordinator root: NSPersistentStoreCoordinator?, configurationName name: String?, at url: NSURL, options options: [NSObject : AnyObject]? = [:])
  func loadMetadata() throws
  weak var persistentStoreCoordinator: @sil_weak NSPersistentStoreCoordinator? { get }
  var configurationName: String { get }
  var options: [NSObject : AnyObject]? { get }
  var url: NSURL?
  var identifier: String!
  var type: String { get }
  var isReadOnly: Bool
  var metadata: [String : AnyObject]!
  func didAdd(to coordinator: NSPersistentStoreCoordinator)
  func willRemove(from coordinator: NSPersistentStoreCoordinator?)
}
struct _objectStoreFlags {
  var isReadOnly: UInt32
  var cleanOnRemove: UInt32
  var isMDDirty: UInt32
  var _RESERVED: UInt32
  init()
  init(isReadOnly isReadOnly: UInt32, cleanOnRemove cleanOnRemove: UInt32, isMDDirty isMDDirty: UInt32, _RESERVED _RESERVED: UInt32)
}

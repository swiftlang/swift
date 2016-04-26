
@available(watchOS 2.0, *)
class NSFetchedResultsController : NSObject {
  init(fetchRequest fetchRequest: NSFetchRequest, managedObjectContext context: NSManagedObjectContext, sectionNameKeyPath sectionNameKeyPath: String?, cacheName name: String?)
  func performFetch() throws
  var fetchRequest: NSFetchRequest { get }
  var managedObjectContext: NSManagedObjectContext { get }
  var sectionNameKeyPath: String? { get }
  var cacheName: String? { get }
  unowned(unsafe) var delegate: @sil_unmanaged NSFetchedResultsControllerDelegate?
  class func deleteCache(withName name: String?)
  var fetchedObjects: [AnyObject]? { get }
  @discardableResult
  func object(at indexPath: NSIndexPath) -> AnyObject
  @discardableResult
  func indexPath(for object: AnyObject) -> NSIndexPath?
  @discardableResult
  func sectionIndexTitle(forSectionName sectionName: String) -> String?
  var sectionIndexTitles: [String] { get }
  var sections: [NSFetchedResultsSectionInfo]? { get }
  @discardableResult
  func section(forSectionIndexTitle title: String, at sectionIndex: Int) -> Int
}
struct _fetchResultsControllerFlags {
  var _sendObjectChangeNotifications: UInt32
  var _sendSectionChangeNotifications: UInt32
  var _sendDidChangeContentNotifications: UInt32
  var _sendWillChangeContentNotifications: UInt32
  var _sendSectionIndexTitleForSectionName: UInt32
  var _changedResultsSinceLastSave: UInt32
  var _hasMutableFetchedResults: UInt32
  var _hasBatchedArrayResults: UInt32
  var _hasSections: UInt32
  var _usesNonpersistedProperties: UInt32
  var _includesSubentities: UInt32
  var _reservedFlags: UInt32
  init()
  init(_sendObjectChangeNotifications _sendObjectChangeNotifications: UInt32, _sendSectionChangeNotifications _sendSectionChangeNotifications: UInt32, _sendDidChangeContentNotifications _sendDidChangeContentNotifications: UInt32, _sendWillChangeContentNotifications _sendWillChangeContentNotifications: UInt32, _sendSectionIndexTitleForSectionName _sendSectionIndexTitleForSectionName: UInt32, _changedResultsSinceLastSave _changedResultsSinceLastSave: UInt32, _hasMutableFetchedResults _hasMutableFetchedResults: UInt32, _hasBatchedArrayResults _hasBatchedArrayResults: UInt32, _hasSections _hasSections: UInt32, _usesNonpersistedProperties _usesNonpersistedProperties: UInt32, _includesSubentities _includesSubentities: UInt32, _reservedFlags _reservedFlags: UInt32)
}
protocol NSFetchedResultsSectionInfo {
  var name: String { get }
  var indexTitle: String? { get }
  var numberOfObjects: Int { get }
  var objects: [AnyObject]? { get }
}
protocol NSFetchedResultsControllerDelegate : NSObjectProtocol {
  @available(watchOS 2.0, *)
  optional func controller(_ controller: NSFetchedResultsController, didChange anObject: AnyObject, at indexPath: NSIndexPath?, for type: NSFetchedResultsChangeType, newIndexPath newIndexPath: NSIndexPath?)
  @available(watchOS 2.0, *)
  optional func controller(_ controller: NSFetchedResultsController, didChange sectionInfo: NSFetchedResultsSectionInfo, atSectionIndex sectionIndex: Int, for type: NSFetchedResultsChangeType)
  @available(watchOS 2.0, *)
  optional func controllerWillChangeContent(_ controller: NSFetchedResultsController)
  @available(watchOS 2.0, *)
  optional func controllerDidChangeContent(_ controller: NSFetchedResultsController)
  @available(watchOS 2.0, *)
  @discardableResult
  optional func controller(_ controller: NSFetchedResultsController, sectionIndexTitleForSectionName sectionName: String) -> String?
}
@available(watchOS 2.0, *)
enum NSFetchedResultsChangeType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case insert
  case delete
  case move
  case update
}

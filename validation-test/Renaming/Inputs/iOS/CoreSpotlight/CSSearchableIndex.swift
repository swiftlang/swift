
@available(iOS 9.0, *)
let CSIndexErrorDomain: String
@available(iOS 9.0, *)
enum CSIndexErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknownError
  case indexUnavailableError
  case invalidItemError
  case invalidClientStateError
  case remoteConnectionError
  case quotaExceeded
  case indexingUnsupported
}
@available(iOS 9.0, *)
class CSSearchableIndex : NSObject {
  weak var indexDelegate: @sil_weak CSSearchableIndexDelegate?
  @discardableResult
  class func isIndexingAvailable() -> Bool
  @discardableResult
  class func defaultSearchableIndex() -> Self
  init(name name: String)
  init(name name: String, protectionClass protectionClass: String?)
  func indexSearchableItems(_ items: [CSSearchableItem], completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func deleteSearchableItems(withIdentifiers identifiers: [String], completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func deleteSearchableItems(withDomainIdentifiers domainIdentifiers: [String], completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func deleteAllSearchableItems(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
}
extension CSSearchableIndex {
  func beginBatch()
  func endBatch(withClientState clientState: NSData, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  func fetchLastClientState(completionHandler completionHandler: (NSData?, NSError?) -> Void)
}
@available(iOS 9.0, *)
protocol CSSearchableIndexDelegate : NSObjectProtocol {
  func searchableIndex(_ searchableIndex: CSSearchableIndex, reindexAllSearchableItemsWithAcknowledgementHandler acknowledgementHandler: () -> Void)
  func searchableIndex(_ searchableIndex: CSSearchableIndex, reindexSearchableItemsWithIdentifiers identifiers: [String], acknowledgementHandler acknowledgementHandler: () -> Void)
  optional func searchableIndexDidThrottle(_ searchableIndex: CSSearchableIndex)
  optional func searchableIndexDidFinishThrottle(_ searchableIndex: CSSearchableIndex)
}

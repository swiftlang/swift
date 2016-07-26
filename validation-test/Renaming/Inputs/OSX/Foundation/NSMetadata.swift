
@available(OSX 10.4, *)
class NSMetadataQuery : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged NSMetadataQueryDelegate?
  @NSCopying var predicate: NSPredicate?
  var sortDescriptors: [NSSortDescriptor]
  var valueListAttributes: [String]
  var groupingAttributes: [String]?
  var notificationBatchingInterval: NSTimeInterval
  var searchScopes: [AnyObject]
  @available(OSX 10.9, *)
  var searchItems: [AnyObject]?
  @available(OSX 10.9, *)
  var operationQueue: NSOperationQueue?
  @discardableResult
  func start() -> Bool
  func stop()
  var isStarted: Bool { get }
  var isGathering: Bool { get }
  var isStopped: Bool { get }
  func disableUpdates()
  func enableUpdates()
  var resultCount: Int { get }
  @discardableResult
  func result(at idx: Int) -> AnyObject
  @available(OSX 10.9, *)
  func enumerateResults(_ block: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.9, *)
  func enumerateResults(_ opts: NSEnumerationOptions = [], using block: (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  var results: [AnyObject] { get }
  @discardableResult
  func index(ofResult result: AnyObject) -> Int
  var valueLists: [String : [NSMetadataQueryAttributeValueTuple]] { get }
  var groupedResults: [NSMetadataQueryResultGroup] { get }
  @discardableResult
  func value(ofAttribute attrName: String, forResultAt idx: Int) -> AnyObject?
}
protocol NSMetadataQueryDelegate : NSObjectProtocol {
  @available(OSX 10.4, *)
  @discardableResult
  optional func metadataQuery(_ query: NSMetadataQuery, replacementObjectForResultObject result: NSMetadataItem) -> AnyObject
  @available(OSX 10.4, *)
  @discardableResult
  optional func metadataQuery(_ query: NSMetadataQuery, replacementValueForAttribute attrName: String, value attrValue: AnyObject) -> AnyObject
}
@available(OSX 10.4, *)
let NSMetadataQueryDidStartGatheringNotification: String
@available(OSX 10.4, *)
let NSMetadataQueryGatheringProgressNotification: String
@available(OSX 10.4, *)
let NSMetadataQueryDidFinishGatheringNotification: String
@available(OSX 10.4, *)
let NSMetadataQueryDidUpdateNotification: String
@available(OSX 10.9, *)
let NSMetadataQueryUpdateAddedItemsKey: String
@available(OSX 10.9, *)
let NSMetadataQueryUpdateChangedItemsKey: String
@available(OSX 10.9, *)
let NSMetadataQueryUpdateRemovedItemsKey: String
@available(OSX 10.4, *)
let NSMetadataQueryResultContentRelevanceAttribute: String
@available(OSX 10.4, *)
let NSMetadataQueryUserHomeScope: String
@available(OSX 10.4, *)
let NSMetadataQueryLocalComputerScope: String
@available(OSX 10.4, *)
let NSMetadataQueryNetworkScope: String
@available(OSX 10.9, *)
let NSMetadataQueryIndexedLocalComputerScope: String
@available(OSX 10.9, *)
let NSMetadataQueryIndexedNetworkScope: String
@available(OSX 10.7, *)
let NSMetadataQueryUbiquitousDocumentsScope: String
@available(OSX 10.7, *)
let NSMetadataQueryUbiquitousDataScope: String
@available(OSX 10.10, *)
let NSMetadataQueryAccessibleUbiquitousExternalDocumentsScope: String
@available(OSX 10.4, *)
class NSMetadataItem : NSObject {
  @available(OSX 10.9, *)
  init?(url url: NSURL)
  @discardableResult
  func value(forAttribute key: String) -> AnyObject?
  @discardableResult
  func values(forAttributes keys: [String]) -> [String : AnyObject]?
  var attributes: [String] { get }
}
@available(OSX 10.4, *)
class NSMetadataQueryAttributeValueTuple : NSObject {
  var attribute: String { get }
  var value: AnyObject? { get }
  var count: Int { get }
}
@available(OSX 10.4, *)
class NSMetadataQueryResultGroup : NSObject {
  var attribute: String { get }
  var value: AnyObject { get }
  var subgroups: [NSMetadataQueryResultGroup]? { get }
  var resultCount: Int { get }
  @discardableResult
  func result(at idx: Int) -> AnyObject
  var results: [AnyObject] { get }
}

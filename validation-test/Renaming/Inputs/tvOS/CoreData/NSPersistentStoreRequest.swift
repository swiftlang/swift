
enum NSPersistentStoreRequestType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case fetchRequestType
  case saveRequestType
  @available(tvOS 8.0, *)
  case batchUpdateRequestType
  @available(tvOS 9.0, *)
  case batchDeleteRequestType
}
@available(tvOS 5.0, *)
class NSPersistentStoreRequest : NSObject, NSCopying {
  var affectedStores: [NSPersistentStore]?
  var requestType: NSPersistentStoreRequestType { get }
}
typealias NSPersistentStoreAsynchronousFetchResultCompletionBlock = (NSAsynchronousFetchResult) -> Void
@available(tvOS 8.0, *)
class NSAsynchronousFetchRequest : NSPersistentStoreRequest {
  var fetchRequest: NSFetchRequest { get }
  var completionBlock: NSPersistentStoreAsynchronousFetchResultCompletionBlock? { get }
  var estimatedResultCount: Int
  init(fetchRequest request: NSFetchRequest, completionBlock blk: NSPersistentStoreAsynchronousFetchResultCompletionBlock? = nil)
}

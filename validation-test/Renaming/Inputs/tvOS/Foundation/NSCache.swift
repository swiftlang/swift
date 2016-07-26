
@available(tvOS 4.0, *)
class NSCache<KeyType : AnyObject, ObjectType : AnyObject> : NSObject {
  var name: String
  unowned(unsafe) var delegate: @sil_unmanaged NSCacheDelegate?
  @discardableResult
  func object(forKey key: KeyType) -> ObjectType?
  func setObject(_ obj: ObjectType, forKey key: KeyType)
  func setObject(_ obj: ObjectType, forKey key: KeyType, cost g: Int)
  func removeObject(forKey key: KeyType)
  func removeAllObjects()
  var totalCostLimit: Int
  var countLimit: Int
  var evictsObjectsWithDiscardedContent: Bool
  init()
}
protocol NSCacheDelegate : NSObjectProtocol {
  @available(tvOS 4.0, *)
  optional func cache(_ cache: NSCache<AnyObject, AnyObject>, willEvictObject obj: AnyObject)
}

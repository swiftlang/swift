
@available(OSX 10.6, *)
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
  @available(OSX 10.6, *)
  optional func cache(_ cache: NSCache<AnyObject, AnyObject>, willEvictObject obj: AnyObject)
}

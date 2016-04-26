
@available(iOS 8.0, *)
class PHCollectionListChangeRequest : NSObject {
  @discardableResult
  class func creationRequestForCollectionList(withTitle title: String) -> Self
  var placeholderForCreatedCollectionList: PHObjectPlaceholder { get }
  class func deleteCollectionLists(_ collectionLists: NSFastEnumeration)
  convenience init?(for collectionList: PHCollectionList)
  convenience init?(for collectionList: PHCollectionList, childCollections childCollections: PHFetchResult<AnyObject>)
  var title: String
  func addChildCollections(_ collections: NSFastEnumeration)
  func insertChildCollections(_ collections: NSFastEnumeration, at indexes: NSIndexSet)
  func removeChildCollections(_ collections: NSFastEnumeration)
  func removeChildCollections(at indexes: NSIndexSet)
  func replaceChildCollections(at indexes: NSIndexSet, withChildCollections collections: NSFastEnumeration)
  func moveChildCollections(at indexes: NSIndexSet, to toIndex: Int)
}

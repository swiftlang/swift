
class PHAssetCollectionChangeRequest : NSObject {
  @discardableResult
  class func creationRequestForAssetCollection(withTitle title: String) -> Self
  var placeholderForCreatedAssetCollection: PHObjectPlaceholder { get }
  class func deleteAssetCollections(_ assetCollections: NSFastEnumeration)
  convenience init?(for assetCollection: PHAssetCollection)
  convenience init?(for assetCollection: PHAssetCollection, assets assets: PHFetchResult<AnyObject>)
  var title: String
  func addAssets(_ assets: NSFastEnumeration)
  func insertAssets(_ assets: NSFastEnumeration, at indexes: NSIndexSet)
  func removeAssets(_ assets: NSFastEnumeration)
  func removeAssets(at indexes: NSIndexSet)
  func replaceAssets(at indexes: NSIndexSet, withAssets assets: NSFastEnumeration)
  func moveAssets(at fromIndexes: NSIndexSet, to toIndex: Int)
}

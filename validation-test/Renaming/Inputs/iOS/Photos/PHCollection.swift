
@available(iOS 8.0, *)
class PHCollection : PHObject {
  var canContainAssets: Bool { get }
  var canContainCollections: Bool { get }
  var localizedTitle: String? { get }
  @discardableResult
  func canPerform(_ anOperation: PHCollectionEditOperation) -> Bool
  @discardableResult
  class func fetchCollections(in collectionList: PHCollectionList, options options: PHFetchOptions?) -> PHFetchResult<PHCollection>
  @discardableResult
  class func fetchTopLevelUserCollections(with options: PHFetchOptions?) -> PHFetchResult<PHCollection>
}
@available(iOS 8.0, *)
class PHAssetCollection : PHCollection {
  var assetCollectionType: PHAssetCollectionType { get }
  var assetCollectionSubtype: PHAssetCollectionSubtype { get }
  var estimatedAssetCount: Int { get }
  var startDate: NSDate? { get }
  var endDate: NSDate? { get }
  var approximateLocation: CLLocation? { get }
  var localizedLocationNames: [String] { get }
  @discardableResult
  class func fetchAssetCollections(withLocalIdentifiers identifiers: [String], options options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func fetchAssetCollections(with type: PHAssetCollectionType, subtype subtype: PHAssetCollectionSubtype, options options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func fetchAssetCollectionsContaining(_ asset: PHAsset, with type: PHAssetCollectionType, options options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func fetchAssetCollections(withALAssetGroupURLs assetGroupURLs: [NSURL], options options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func fetchMoments(inMomentList momentList: PHCollectionList, options options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func fetchMoments(with options: PHFetchOptions?) -> PHFetchResult<PHAssetCollection>
  @discardableResult
  class func transientAssetCollection(with assets: [PHAsset], title title: String?) -> PHAssetCollection
  @discardableResult
  class func transientAssetCollection(withAssetFetchResult fetchResult: PHFetchResult<PHAsset>, title title: String?) -> PHAssetCollection
}
@available(iOS 8.0, *)
class PHCollectionList : PHCollection {
  var collectionListType: PHCollectionListType { get }
  var collectionListSubtype: PHCollectionListSubtype { get }
  var startDate: NSDate? { get }
  var endDate: NSDate? { get }
  var localizedLocationNames: [String] { get }
  @discardableResult
  class func fetchCollectionListsContaining(_ collection: PHCollection, options options: PHFetchOptions?) -> PHFetchResult<PHCollectionList>
  @discardableResult
  class func fetchCollectionLists(withLocalIdentifiers identifiers: [String], options options: PHFetchOptions?) -> PHFetchResult<PHCollectionList>
  @discardableResult
  class func fetchCollectionLists(with collectionListType: PHCollectionListType, subtype subtype: PHCollectionListSubtype, options options: PHFetchOptions?) -> PHFetchResult<PHCollectionList>
  @discardableResult
  class func fetchMomentLists(with momentListSubtype: PHCollectionListSubtype, containingMoment moment: PHAssetCollection, options options: PHFetchOptions?) -> PHFetchResult<PHCollectionList>
  @discardableResult
  class func fetchMomentLists(with momentListSubtype: PHCollectionListSubtype, options options: PHFetchOptions?) -> PHFetchResult<PHCollectionList>
  @discardableResult
  class func transientCollectionList(with collections: [PHCollection], title title: String?) -> PHCollectionList
  @discardableResult
  class func transientCollectionList(withCollectionsFetchResult fetchResult: PHFetchResult<PHCollection>, title title: String?) -> PHCollectionList
}

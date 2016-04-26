
@available(iOS 8.0, *)
class PHAsset : PHObject {
  var mediaType: PHAssetMediaType { get }
  var mediaSubtypes: PHAssetMediaSubtype { get }
  var pixelWidth: Int { get }
  var pixelHeight: Int { get }
  var creationDate: NSDate? { get }
  var modificationDate: NSDate? { get }
  var location: CLLocation? { get }
  var duration: NSTimeInterval { get }
  var isHidden: Bool { get }
  var isFavorite: Bool { get }
  var burstIdentifier: String? { get }
  var burstSelectionTypes: PHAssetBurstSelectionType { get }
  var representsBurst: Bool { get }
  @available(iOS 9.0, *)
  var sourceType: PHAssetSourceType { get }
  @discardableResult
  func canPerform(_ editOperation: PHAssetEditOperation) -> Bool
  @discardableResult
  class func fetchAssets(in assetCollection: PHAssetCollection, options options: PHFetchOptions?) -> PHFetchResult<PHAsset>
  @discardableResult
  class func fetchAssets(withLocalIdentifiers identifiers: [String], options options: PHFetchOptions?) -> PHFetchResult<PHAsset>
  @discardableResult
  class func fetchKeyAssets(in assetCollection: PHAssetCollection, options options: PHFetchOptions?) -> PHFetchResult<PHAsset>?
  @discardableResult
  class func fetchAssets(withBurstIdentifier burstIdentifier: String, options options: PHFetchOptions?) -> PHFetchResult<PHAsset>
  @discardableResult
  class func fetchAssets(with options: PHFetchOptions?) -> PHFetchResult<PHAsset>
  @discardableResult
  class func fetchAssets(with mediaType: PHAssetMediaType, options options: PHFetchOptions?) -> PHFetchResult<PHAsset>
  @discardableResult
  class func fetchAssets(withALAssetURLs assetURLs: [NSURL], options options: PHFetchOptions?) -> PHFetchResult<PHAsset>
}

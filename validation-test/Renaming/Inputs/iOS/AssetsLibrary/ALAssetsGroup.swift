
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the PHFetchResult returned by fetchAssetsInAssetCollection:options: on PHAsset from the Photos framework to enumerate the assets in an asset collection instead")
typealias ALAssetsGroupEnumerationResultsBlock = (ALAsset!, Int, UnsafeMutablePointer<ObjCBool>!) -> Void
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the localizedTitle property on a PHAssetCollection from the Photos framework instead")
let ALAssetsGroupPropertyName: String
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the assetCollectionType and assetCollectionSubtype properties on a PHAssetCollection from the Photos framework instead")
let ALAssetsGroupPropertyType: String
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the localIdentifier property on a PHAssetCollection from the Photos framework instead")
let ALAssetsGroupPropertyPersistentID: String
@available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use the localIdentifier property on a PHAssetCollection from the Photos framework, or to lookup PHAssetCollections by a previously known ALAssetsGroupPropertyURL use fetchAssetCollectionsWithALAssetGroupURLs:options: instead")
let ALAssetsGroupPropertyURL: String
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use PHAssetCollection from the Photos framework instead")
class ALAssetsGroup : NSObject {
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the class properties on a PHAssetCollection in the Photos framework instead")
  @discardableResult
  func value(forProperty property: String!) -> AnyObject!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use fetchKeyAssetsInAssetCollection:options: on PHAsset, then use the PHImageManager to request image data for key assets in the asset collection from the Photos framework instead")
  @discardableResult
  func posterImage() -> Unmanaged<CGImage>!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use fetchAssetsInAssetCollection:options: on PHAsset with a predicate in the PHFetchOptions from the Photos framework to filter the assets in an asset collection instead")
  func setAssetsFilter(_ filter: ALAssetsFilter!)
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the estimatedAssetCount on PHAssetCollection for a quick estimate of the total assets in a collection (or fetch the assets to get an exact value) from the Photos framework instead")
  @discardableResult
  func numberOfAssets() -> Int
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the PHFetchResult returned by fetchAssetsInAssetCollection:options: on PHAsset to enumerate the assets in an asset collection from the Photos framework instead")
  func enumerateAssets(_ enumerationBlock: ALAssetsGroupEnumerationResultsBlock!)
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the PHFetchResult returned by fetchAssetsInAssetCollection:options: on PHAsset to enumerate the assets in an asset collection from the Photos framework instead")
  func enumerateAssets(_ options: NSEnumerationOptions = [], using enumerationBlock: ALAssetsGroupEnumerationResultsBlock!)
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the PHFetchResult returned by fetchAssetsInAssetCollection:options: on PHAsset to enumerate the assets in an asset collection from the Photos framework instead")
  func enumerateAssets(at indexSet: NSIndexSet!, options options: NSEnumerationOptions = [], using enumerationBlock: ALAssetsGroupEnumerationResultsBlock!)
  @available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use canPerformEditOperation: on a PHAssetCollection from the Photos framework instead")
  var isEditable: Bool { get }
  @available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use addAssets: on a PHAssetCollectionChangeRequest: created from a PHAssetCollection in the Photos framework instead")
  @discardableResult
  func add(_ asset: ALAsset!) -> Bool
}

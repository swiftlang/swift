
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use fetchAssetsInAssetCollection:options: on PHAsset and set a mediaType predicate on the PHFetchOptions from the Photos framework instead")
class ALAssetsFilter : NSObject {
  @discardableResult
  class func allPhotos() -> ALAssetsFilter!
  @discardableResult
  class func allVideos() -> ALAssetsFilter!
  @discardableResult
  class func allAssets() -> ALAssetsFilter!
}

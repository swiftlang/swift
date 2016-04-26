
@available(iOS 9.0, *)
class PHAssetResource : NSObject {
  var type: PHAssetResourceType { get }
  var assetLocalIdentifier: String { get }
  var uniformTypeIdentifier: String { get }
  var originalFilename: String { get }
  @discardableResult
  class func assetResources(for asset: PHAsset) -> [PHAssetResource]
  @available(iOS 9.1, *)
  @discardableResult
  class func assetResources(for livePhoto: PHLivePhoto) -> [PHAssetResource]
}

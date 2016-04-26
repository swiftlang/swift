
@available(iOS 8.0, *)
enum PHImageRequestOptionsVersion : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case current
  case unadjusted
  case original
}
@available(iOS 8.0, *)
enum PHImageRequestOptionsDeliveryMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case opportunistic
  case highQualityFormat
  case fastFormat
}
@available(iOS 8.0, *)
enum PHImageRequestOptionsResizeMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case fast
  case exact
}
@available(iOS 8.0, *)
typealias PHAssetImageProgressHandler = (Double, NSError?, UnsafeMutablePointer<ObjCBool>, [NSObject : AnyObject]?) -> Void
@available(iOS 8.0, *)
class PHImageRequestOptions : NSObject, NSCopying {
  var version: PHImageRequestOptionsVersion
  var deliveryMode: PHImageRequestOptionsDeliveryMode
  var resizeMode: PHImageRequestOptionsResizeMode
  var normalizedCropRect: CGRect
  var isNetworkAccessAllowed: Bool
  var isSynchronous: Bool
  var progressHandler: PHAssetImageProgressHandler?
}
@available(iOS 9.1, *)
class PHLivePhotoRequestOptions : NSObject, NSCopying {
  var deliveryMode: PHImageRequestOptionsDeliveryMode
  var isNetworkAccessAllowed: Bool
  var progressHandler: PHAssetImageProgressHandler?
}
@available(iOS 8.0, *)
enum PHVideoRequestOptionsVersion : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case current
  case original
}
@available(iOS 8.0, *)
enum PHVideoRequestOptionsDeliveryMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case highQualityFormat
  case mediumQualityFormat
  case fastFormat
}
@available(iOS 8.0, *)
typealias PHAssetVideoProgressHandler = (Double, NSError?, UnsafeMutablePointer<ObjCBool>, [NSObject : AnyObject]?) -> Void
@available(iOS 8.0, *)
class PHVideoRequestOptions : NSObject {
  var isNetworkAccessAllowed: Bool
  var version: PHVideoRequestOptionsVersion
  var deliveryMode: PHVideoRequestOptionsDeliveryMode
  var progressHandler: PHAssetVideoProgressHandler?
}
@available(iOS 8.0, *)
typealias PHImageRequestID = Int32
let PHInvalidImageRequestID: PHImageRequestID
@available(iOS 8.0, *)
let PHImageManagerMaximumSize: CGSize
@available(iOS 8.0, *)
let PHImageResultIsInCloudKey: String
@available(iOS 8.0, *)
let PHImageResultIsDegradedKey: String
@available(iOS 8.0, *)
let PHImageResultRequestIDKey: String
@available(iOS 8.0, *)
let PHImageCancelledKey: String
@available(iOS 8.0, *)
let PHImageErrorKey: String
@available(iOS 8.0, *)
class PHImageManager : NSObject {
  @discardableResult
  class func defaultManager() -> PHImageManager
  @discardableResult
  func requestImage(for asset: PHAsset, targetSize targetSize: CGSize, contentMode contentMode: PHImageContentMode, options options: PHImageRequestOptions?, resultHandler resultHandler: (UIImage?, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
  @discardableResult
  func requestImageData(for asset: PHAsset, options options: PHImageRequestOptions?, resultHandler resultHandler: (NSData?, String?, UIImageOrientation, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
  func cancelImageRequest(_ requestID: PHImageRequestID)
  @available(iOS 9.1, *)
  @discardableResult
  func requestLivePhoto(for asset: PHAsset, targetSize targetSize: CGSize, contentMode contentMode: PHImageContentMode, options options: PHLivePhotoRequestOptions?, resultHandler resultHandler: (PHLivePhoto?, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
  @discardableResult
  func requestPlayerItem(forVideo asset: PHAsset, options options: PHVideoRequestOptions?, resultHandler resultHandler: (AVPlayerItem?, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
  @discardableResult
  func requestExportSession(forVideo asset: PHAsset, options options: PHVideoRequestOptions?, exportPreset exportPreset: String, resultHandler resultHandler: (AVAssetExportSession?, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
  @discardableResult
  func requestAVAsset(forVideo asset: PHAsset, options options: PHVideoRequestOptions?, resultHandler resultHandler: (AVAsset?, AVAudioMix?, [NSObject : AnyObject]?) -> Void) -> PHImageRequestID
}
@available(iOS 8.0, *)
class PHCachingImageManager : PHImageManager {
  var allowsCachingHighQualityImages: Bool
  func startCachingImages(for assets: [PHAsset], targetSize targetSize: CGSize, contentMode contentMode: PHImageContentMode, options options: PHImageRequestOptions?)
  func stopCachingImages(for assets: [PHAsset], targetSize targetSize: CGSize, contentMode contentMode: PHImageContentMode, options options: PHImageRequestOptions?)
  func stopCachingImagesForAllAssets()
}

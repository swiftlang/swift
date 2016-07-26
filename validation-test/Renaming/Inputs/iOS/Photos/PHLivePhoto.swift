
@available(iOS 9.1, *)
typealias PHLivePhotoRequestID = Int32
let PHLivePhotoRequestIDInvalid: PHLivePhotoRequestID
@available(iOS 9.1, *)
let PHLivePhotoInfoErrorKey: String
@available(iOS 9.1, *)
let PHLivePhotoInfoIsDegradedKey: String
@available(iOS 9.1, *)
let PHLivePhotoInfoCancelledKey: String
@available(iOS 9.1, *)
class PHLivePhoto : NSObject, NSCopying, NSSecureCoding {
  var size: CGSize { get }
  @discardableResult
  class func request(withResourceFileURLs fileURLs: [NSURL], placeholderImage image: UIImage?, targetSize targetSize: CGSize, contentMode contentMode: PHImageContentMode, resultHandler resultHandler: (PHLivePhoto?, [NSObject : AnyObject]) -> Void) -> PHLivePhotoRequestID
  class func cancelRequest(withRequestID requestID: PHLivePhotoRequestID)
}

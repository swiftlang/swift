
@available(iOS 8.0, *)
class PHAssetChangeRequest : NSObject {
  @discardableResult
  class func creationRequestForAsset(from image: UIImage) -> Self
  @discardableResult
  class func creationRequestForAssetFromImage(atFileURL fileURL: NSURL) -> Self?
  @discardableResult
  class func creationRequestForAssetFromVideo(atFileURL fileURL: NSURL) -> Self?
  var placeholderForCreatedAsset: PHObjectPlaceholder? { get }
  class func deleteAssets(_ assets: NSFastEnumeration)
  convenience init(for asset: PHAsset)
  var creationDate: NSDate?
  var location: CLLocation?
  var isFavorite: Bool
  var isHidden: Bool
  var contentEditingOutput: PHContentEditingOutput?
  func revertAssetContentToOriginal()
}
@available(iOS 8.0, *)
typealias PHContentEditingInputRequestID = Int
@available(iOS 8.0, *)
class PHContentEditingInputRequestOptions : NSObject {
  var canHandleAdjustmentData: (PHAdjustmentData) -> Bool
  var isNetworkAccessAllowed: Bool
  var progressHandler: ((Double, UnsafeMutablePointer<ObjCBool>) -> Void)?
}
extension PHAsset {
  @available(iOS 8.0, *)
  @discardableResult
  func requestContentEditingInput(with options: PHContentEditingInputRequestOptions?, completionHandler completionHandler: (PHContentEditingInput?, [NSObject : AnyObject]) -> Void) -> PHContentEditingInputRequestID
  @available(iOS 8.0, *)
  func cancelContentEditingInputRequest(_ requestID: PHContentEditingInputRequestID)
}
@available(iOS 8.0, *)
let PHContentEditingInputResultIsInCloudKey: String
@available(iOS 8.0, *)
let PHContentEditingInputCancelledKey: String
@available(iOS 8.0, *)
let PHContentEditingInputErrorKey: String
extension PHContentEditingOutput {
  init(placeholderForCreatedAsset placeholderForCreatedAsset: PHObjectPlaceholder)
}

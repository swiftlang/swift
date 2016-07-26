
@available(iOS 8.0, *)
class PHContentEditingInput : NSObject {
  var mediaType: PHAssetMediaType { get }
  var mediaSubtypes: PHAssetMediaSubtype { get }
  @NSCopying var creationDate: NSDate? { get }
  @NSCopying var location: CLLocation? { get }
  var uniformTypeIdentifier: String? { get }
  var adjustmentData: PHAdjustmentData { get }
  var displaySizeImage: UIImage? { get }
  @NSCopying var fullSizeImageURL: NSURL? { get }
  var fullSizeImageOrientation: Int32 { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  var avAsset: AVAsset? { get }
  @available(iOS 9.0, *)
  var audiovisualAsset: AVAsset? { get }
}

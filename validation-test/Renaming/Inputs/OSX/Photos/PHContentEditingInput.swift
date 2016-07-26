
@available(OSX 10.11, *)
class PHContentEditingInput : NSObject {
  var mediaType: PHAssetMediaType { get }
  var mediaSubtypes: PHAssetMediaSubtype { get }
  @NSCopying var creationDate: NSDate? { get }
  var uniformTypeIdentifier: String? { get }
  var adjustmentData: PHAdjustmentData { get }
  @NSCopying var fullSizeImageURL: NSURL? { get }
  var fullSizeImageOrientation: Int32 { get }
}

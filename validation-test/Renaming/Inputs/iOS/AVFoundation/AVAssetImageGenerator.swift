
@available(iOS 4.0, *)
let AVAssetImageGeneratorApertureModeCleanAperture: String
@available(iOS 4.0, *)
let AVAssetImageGeneratorApertureModeProductionAperture: String
@available(iOS 4.0, *)
let AVAssetImageGeneratorApertureModeEncodedPixels: String
enum AVAssetImageGeneratorResult : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case succeeded
  case failed
  case cancelled
}
@available(iOS 4.0, *)
class AVAssetImageGenerator : NSObject {
  @available(iOS 6.0, *)
  var asset: AVAsset { get }
  var appliesPreferredTrackTransform: Bool
  var maximumSize: CGSize
  var apertureMode: String?
  @NSCopying var videoComposition: AVVideoComposition?
  @available(iOS 7.0, *)
  var customVideoCompositor: AVVideoCompositing? { get }
  @available(iOS 5.0, *)
  var requestedTimeToleranceBefore: CMTime
  @available(iOS 5.0, *)
  var requestedTimeToleranceAfter: CMTime
  init(asset asset: AVAsset)
  @discardableResult
  func copyCGImage(at requestedTime: CMTime, actualTime actualTime: UnsafeMutablePointer<CMTime>?) throws -> CGImage
  func generateCGImagesAsynchronously(forTimes requestedTimes: [NSValue], completionHandler handler: AVAssetImageGeneratorCompletionHandler)
  func cancelAllCGImageGeneration()
}
typealias AVAssetImageGeneratorCompletionHandler = (CMTime, CGImage?, CMTime, AVAssetImageGeneratorResult, NSError?) -> Void


@available(iOS 4.0, *)
let AVAssetExportPresetLowQuality: String
@available(iOS 4.0, *)
let AVAssetExportPresetMediumQuality: String
@available(iOS 4.0, *)
let AVAssetExportPresetHighestQuality: String
@available(iOS 4.0, *)
let AVAssetExportPreset640x480: String
@available(iOS 4.0, *)
let AVAssetExportPreset960x540: String
@available(iOS 4.0, *)
let AVAssetExportPreset1280x720: String
@available(iOS 5.0, *)
let AVAssetExportPreset1920x1080: String
@available(iOS 9.0, *)
let AVAssetExportPreset3840x2160: String
@available(iOS 4.0, *)
let AVAssetExportPresetAppleM4A: String
@available(iOS 4.0, *)
let AVAssetExportPresetPassthrough: String
enum AVAssetExportSessionStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case waiting
  case exporting
  case completed
  case failed
  case cancelled
}
@available(iOS 4.0, *)
class AVAssetExportSession : NSObject {
  init?(asset asset: AVAsset, presetName presetName: String)
  var presetName: String { get }
  @available(iOS 5.0, *)
  var asset: AVAsset { get }
  var outputFileType: String?
  @NSCopying var outputURL: NSURL?
  var shouldOptimizeForNetworkUse: Bool
  var status: AVAssetExportSessionStatus { get }
  var error: NSError? { get }
  func exportAsynchronously(completionHandler handler: () -> Void)
  var progress: Float { get }
  func cancelExport()
}
extension AVAssetExportSession {
  @discardableResult
  class func allExportPresets() -> [String]
  @discardableResult
  class func exportPresets(compatibleWith asset: AVAsset) -> [String]
  @available(iOS 6.0, *)
  class func determineCompatibility(ofExportPreset presetName: String, with asset: AVAsset, outputFileType outputFileType: String?, completionHandler handler: (Bool) -> Void)
}
extension AVAssetExportSession {
  var supportedFileTypes: [String] { get }
  @available(iOS 6.0, *)
  func determineCompatibleFileTypes(completionHandler handler: ([String]) -> Void)
}
extension AVAssetExportSession {
  var timeRange: CMTimeRange
  @available(iOS 4.0, *)
  var maxDuration: CMTime { get }
  @available(iOS 5.0, *)
  var estimatedOutputFileLength: Int64 { get }
  @available(iOS 4.0, *)
  var fileLengthLimit: Int64
}
extension AVAssetExportSession {
  var metadata: [AVMetadataItem]?
  @available(iOS 7.0, *)
  var metadataItemFilter: AVMetadataItemFilter?
}
extension AVAssetExportSession {
  @available(iOS 7.0, *)
  var audioTimePitchAlgorithm: String
  @NSCopying var audioMix: AVAudioMix?
  @NSCopying var videoComposition: AVVideoComposition?
  @available(iOS 7.0, *)
  var customVideoCompositor: AVVideoCompositing? { get }
}
extension AVAssetExportSession {
  @available(iOS 8.0, *)
  var canPerformMultiplePassesOverSourceMediaData: Bool
  @available(iOS 8.0, *)
  @NSCopying var directoryForTemporaryFiles: NSURL?
}

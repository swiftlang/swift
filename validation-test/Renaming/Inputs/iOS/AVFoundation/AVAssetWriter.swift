
enum AVAssetWriterStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case writing
  case completed
  case failed
  case cancelled
}
@available(iOS 4.1, *)
class AVAssetWriter : NSObject {
  init(url outputURL: NSURL, fileType outputFileType: String) throws
  @NSCopying var outputURL: NSURL { get }
  var outputFileType: String { get }
  var availableMediaTypes: [String] { get }
  var status: AVAssetWriterStatus { get }
  var error: NSError? { get }
  var metadata: [AVMetadataItem]
  var shouldOptimizeForNetworkUse: Bool
  @available(iOS 8.0, *)
  @NSCopying var directoryForTemporaryFiles: NSURL?
  var inputs: [AVAssetWriterInput] { get }
  @discardableResult
  func canApplyOutputSettings(_ outputSettings: [String : AnyObject]?, forMediaType mediaType: String) -> Bool
  @discardableResult
  func canAddInput(_ input: AVAssetWriterInput) -> Bool
  func addInput(_ input: AVAssetWriterInput)
  @discardableResult
  func startWriting() -> Bool
  func startSession(atSourceTime startTime: CMTime)
  func endSession(atSourceTime endTime: CMTime)
  func cancelWriting()
  @available(iOS 6.0, *)
  func finishWriting(completionHandler handler: () -> Void)
}
extension AVAssetWriter {
  var movieFragmentInterval: CMTime
  var overallDurationHint: CMTime
  @available(iOS 4.3, *)
  var movieTimeScale: CMTimeScale
}
extension AVAssetWriter {
  @available(iOS 7.0, *)
  @discardableResult
  func canAddInputGroup(_ inputGroup: AVAssetWriterInputGroup) -> Bool
  @available(iOS 7.0, *)
  func addInputGroup(_ inputGroup: AVAssetWriterInputGroup)
  @available(iOS 7.0, *)
  var inputGroups: [AVAssetWriterInputGroup] { get }
}
@available(iOS 7.0, *)
class AVAssetWriterInputGroup : AVMediaSelectionGroup {
  init(inputs inputs: [AVAssetWriterInput], defaultInput defaultInput: AVAssetWriterInput?)
  var inputs: [AVAssetWriterInput] { get }
  var defaultInput: AVAssetWriterInput? { get }
}

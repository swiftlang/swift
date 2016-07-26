
@available(tvOS 4.1, *)
class AVAssetWriterInput : NSObject {
  convenience init(mediaType mediaType: String, outputSettings outputSettings: [String : AnyObject]?)
  @available(tvOS 6.0, *)
  init(mediaType mediaType: String, outputSettings outputSettings: [String : AnyObject]?, sourceFormatHint sourceFormatHint: CMFormatDescription?)
  var mediaType: String { get }
  var outputSettings: [String : AnyObject]? { get }
  @available(tvOS 6.0, *)
  var sourceFormatHint: CMFormatDescription? { get }
  var metadata: [AVMetadataItem]
  var isReadyForMoreMediaData: Bool { get }
  var expectsMediaDataInRealTime: Bool
  func requestMediaDataWhenReady(on queue: dispatch_queue_t, using block: () -> Void)
  @discardableResult
  func appendSampleBuffer(_ sampleBuffer: CMSampleBuffer) -> Bool
  func markAsFinished()
}
extension AVAssetWriterInput {
  @available(tvOS 7.0, *)
  var languageCode: String?
  @available(tvOS 7.0, *)
  var extendedLanguageTag: String?
}
extension AVAssetWriterInput {
  @available(tvOS 7.0, *)
  var naturalSize: CGSize
  var transform: CGAffineTransform
}
extension AVAssetWriterInput {
  @available(tvOS 7.0, *)
  var preferredVolume: Float
}
extension AVAssetWriterInput {
  @available(tvOS 7.0, *)
  var marksOutputTrackAsEnabled: Bool
  @available(tvOS 4.3, *)
  var mediaTimeScale: CMTimeScale
  @available(tvOS 8.0, *)
  var preferredMediaChunkDuration: CMTime
  @available(tvOS 8.0, *)
  var preferredMediaChunkAlignment: Int
  @available(tvOS 8.0, *)
  @NSCopying var sampleReferenceBaseURL: NSURL?
}
extension AVAssetWriterInput {
  @available(tvOS 7.0, *)
  @discardableResult
  func canAddTrackAssociation(withTrackOf input: AVAssetWriterInput, type trackAssociationType: String) -> Bool
  @available(tvOS 7.0, *)
  func addTrackAssociation(withTrackOf input: AVAssetWriterInput, type trackAssociationType: String)
}
extension AVAssetWriterInput {
  @available(tvOS 8.0, *)
  var performsMultiPassEncodingIfSupported: Bool
  @available(tvOS 8.0, *)
  var canPerformMultiplePasses: Bool { get }
  @available(tvOS 8.0, *)
  var currentPassDescription: AVAssetWriterInputPassDescription? { get }
  @available(tvOS 8.0, *)
  func respondToEachPassDescription(on queue: dispatch_queue_t, using block: dispatch_block_t)
  @available(tvOS 8.0, *)
  func markCurrentPassAsFinished()
}
@available(tvOS 8.0, *)
class AVAssetWriterInputPassDescription : NSObject {
  var sourceTimeRanges: [NSValue] { get }
}
@available(tvOS 4.1, *)
class AVAssetWriterInputPixelBufferAdaptor : NSObject {
  init(assetWriterInput input: AVAssetWriterInput, sourcePixelBufferAttributes sourcePixelBufferAttributes: [String : AnyObject]? = [:])
  var assetWriterInput: AVAssetWriterInput { get }
  var sourcePixelBufferAttributes: [String : AnyObject]? { get }
  var pixelBufferPool: CVPixelBufferPool? { get }
  @discardableResult
  func appendPixelBuffer(_ pixelBuffer: CVPixelBuffer, withPresentationTime presentationTime: CMTime) -> Bool
}
@available(tvOS 8.0, *)
class AVAssetWriterInputMetadataAdaptor : NSObject {
  init(assetWriterInput input: AVAssetWriterInput)
  var assetWriterInput: AVAssetWriterInput { get }
  @discardableResult
  func appendTimedMetadataGroup(_ timedMetadataGroup: AVTimedMetadataGroup) -> Bool
}

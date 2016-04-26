
@available(OSX 10.7, *)
class AVAssetWriterInput : NSObject {
  convenience init(mediaType mediaType: String, outputSettings outputSettings: [String : AnyObject]?)
  @available(OSX 10.8, *)
  init(mediaType mediaType: String, outputSettings outputSettings: [String : AnyObject]?, sourceFormatHint sourceFormatHint: CMFormatDescription?)
  var mediaType: String { get }
  var outputSettings: [String : AnyObject]? { get }
  @available(OSX 10.8, *)
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
  @available(OSX 10.9, *)
  var languageCode: String?
  @available(OSX 10.9, *)
  var extendedLanguageTag: String?
}
extension AVAssetWriterInput {
  @available(OSX 10.9, *)
  var naturalSize: CGSize
  var transform: CGAffineTransform
}
extension AVAssetWriterInput {
  @available(OSX 10.9, *)
  var preferredVolume: Float
}
extension AVAssetWriterInput {
  @available(OSX 10.9, *)
  var marksOutputTrackAsEnabled: Bool
  @available(OSX 10.7, *)
  var mediaTimeScale: CMTimeScale
  @available(OSX 10.10, *)
  var preferredMediaChunkDuration: CMTime
  @available(OSX 10.10, *)
  var preferredMediaChunkAlignment: Int
  @available(OSX 10.10, *)
  @NSCopying var sampleReferenceBaseURL: NSURL?
}
extension AVAssetWriterInput {
  @available(OSX 10.9, *)
  @discardableResult
  func canAddTrackAssociation(withTrackOf input: AVAssetWriterInput, type trackAssociationType: String) -> Bool
  @available(OSX 10.9, *)
  func addTrackAssociation(withTrackOf input: AVAssetWriterInput, type trackAssociationType: String)
}
extension AVAssetWriterInput {
  @available(OSX 10.10, *)
  var performsMultiPassEncodingIfSupported: Bool
  @available(OSX 10.10, *)
  var canPerformMultiplePasses: Bool { get }
  @available(OSX 10.10, *)
  var currentPassDescription: AVAssetWriterInputPassDescription? { get }
  @available(OSX 10.10, *)
  func respondToEachPassDescription(on queue: dispatch_queue_t, using block: dispatch_block_t)
  @available(OSX 10.10, *)
  func markCurrentPassAsFinished()
}
@available(OSX 10.10, *)
class AVAssetWriterInputPassDescription : NSObject {
  var sourceTimeRanges: [NSValue] { get }
}
@available(OSX 10.7, *)
class AVAssetWriterInputPixelBufferAdaptor : NSObject {
  init(assetWriterInput input: AVAssetWriterInput, sourcePixelBufferAttributes sourcePixelBufferAttributes: [String : AnyObject]? = [:])
  var assetWriterInput: AVAssetWriterInput { get }
  var sourcePixelBufferAttributes: [String : AnyObject]? { get }
  var pixelBufferPool: CVPixelBufferPool? { get }
  @discardableResult
  func appendPixelBuffer(_ pixelBuffer: CVPixelBuffer, withPresentationTime presentationTime: CMTime) -> Bool
}
@available(OSX 10.10, *)
class AVAssetWriterInputMetadataAdaptor : NSObject {
  init(assetWriterInput input: AVAssetWriterInput)
  var assetWriterInput: AVAssetWriterInput { get }
  @discardableResult
  func appendTimedMetadataGroup(_ timedMetadataGroup: AVTimedMetadataGroup) -> Bool
}

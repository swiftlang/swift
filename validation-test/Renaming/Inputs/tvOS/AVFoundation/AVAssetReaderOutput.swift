
@available(tvOS 4.1, *)
class AVAssetReaderOutput : NSObject {
  var mediaType: String { get }
  @available(tvOS 5.0, *)
  var alwaysCopiesSampleData: Bool
  @discardableResult
  func copyNextSampleBuffer() -> CMSampleBuffer?
}
extension AVAssetReaderOutput {
  @available(tvOS 8.0, *)
  var supportsRandomAccess: Bool
  @available(tvOS 8.0, *)
  func reset(forReadingTimeRanges timeRanges: [NSValue])
  @available(tvOS 8.0, *)
  func markConfigurationAsFinal()
}
@available(tvOS 4.1, *)
class AVAssetReaderTrackOutput : AVAssetReaderOutput {
  init(track track: AVAssetTrack, outputSettings outputSettings: [String : AnyObject]?)
  var track: AVAssetTrack { get }
  var outputSettings: [String : AnyObject]? { get }
  @available(tvOS 7.0, *)
  var audioTimePitchAlgorithm: String
}
@available(tvOS 4.1, *)
class AVAssetReaderAudioMixOutput : AVAssetReaderOutput {
  init(audioTracks audioTracks: [AVAssetTrack], audioSettings audioSettings: [String : AnyObject]?)
  var audioTracks: [AVAssetTrack] { get }
  var audioSettings: [String : AnyObject]? { get }
  @NSCopying var audioMix: AVAudioMix?
  @available(tvOS 7.0, *)
  var audioTimePitchAlgorithm: String
}
@available(tvOS 4.1, *)
class AVAssetReaderVideoCompositionOutput : AVAssetReaderOutput {
  init(videoTracks videoTracks: [AVAssetTrack], videoSettings videoSettings: [String : AnyObject]?)
  var videoTracks: [AVAssetTrack] { get }
  var videoSettings: [String : AnyObject]? { get }
  @NSCopying var videoComposition: AVVideoComposition?
  @available(tvOS 7.0, *)
  var customVideoCompositor: AVVideoCompositing? { get }
}
@available(tvOS 8.0, *)
class AVAssetReaderOutputMetadataAdaptor : NSObject {
  init(assetReaderTrackOutput trackOutput: AVAssetReaderTrackOutput)
  var assetReaderTrackOutput: AVAssetReaderTrackOutput { get }
  @discardableResult
  func nextTimedMetadataGroup() -> AVTimedMetadataGroup?
}
@available(tvOS 8.0, *)
class AVAssetReaderSampleReferenceOutput : AVAssetReaderOutput {
  init(track track: AVAssetTrack)
  var track: AVAssetTrack { get }
}

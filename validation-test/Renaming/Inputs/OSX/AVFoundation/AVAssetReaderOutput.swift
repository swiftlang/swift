
@available(OSX 10.7, *)
class AVAssetReaderOutput : NSObject {
  var mediaType: String { get }
  @available(OSX 10.8, *)
  var alwaysCopiesSampleData: Bool
  @discardableResult
  func copyNextSampleBuffer() -> CMSampleBuffer?
}
extension AVAssetReaderOutput {
  @available(OSX 10.10, *)
  var supportsRandomAccess: Bool
  @available(OSX 10.10, *)
  func reset(forReadingTimeRanges timeRanges: [NSValue])
  @available(OSX 10.10, *)
  func markConfigurationAsFinal()
}
@available(OSX 10.7, *)
class AVAssetReaderTrackOutput : AVAssetReaderOutput {
  init(track track: AVAssetTrack, outputSettings outputSettings: [String : AnyObject]?)
  var track: AVAssetTrack { get }
  var outputSettings: [String : AnyObject]? { get }
  @available(OSX 10.9, *)
  var audioTimePitchAlgorithm: String
}
@available(OSX 10.7, *)
class AVAssetReaderAudioMixOutput : AVAssetReaderOutput {
  init(audioTracks audioTracks: [AVAssetTrack], audioSettings audioSettings: [String : AnyObject]?)
  var audioTracks: [AVAssetTrack] { get }
  var audioSettings: [String : AnyObject]? { get }
  @NSCopying var audioMix: AVAudioMix?
  @available(OSX 10.9, *)
  var audioTimePitchAlgorithm: String
}
@available(OSX 10.7, *)
class AVAssetReaderVideoCompositionOutput : AVAssetReaderOutput {
  init(videoTracks videoTracks: [AVAssetTrack], videoSettings videoSettings: [String : AnyObject]?)
  var videoTracks: [AVAssetTrack] { get }
  var videoSettings: [String : AnyObject]? { get }
  @NSCopying var videoComposition: AVVideoComposition?
  @available(OSX 10.9, *)
  var customVideoCompositor: AVVideoCompositing? { get }
}
@available(OSX 10.10, *)
class AVAssetReaderOutputMetadataAdaptor : NSObject {
  init(assetReaderTrackOutput trackOutput: AVAssetReaderTrackOutput)
  var assetReaderTrackOutput: AVAssetReaderTrackOutput { get }
  @discardableResult
  func nextTimedMetadataGroup() -> AVTimedMetadataGroup?
}
@available(OSX 10.10, *)
class AVAssetReaderSampleReferenceOutput : AVAssetReaderOutput {
  init(track track: AVAssetTrack)
  var track: AVAssetTrack { get }
}

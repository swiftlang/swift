
@available(tvOS 4.0, *)
class AVCompositionTrack : AVAssetTrack {
}
@available(tvOS 4.0, *)
class AVMutableCompositionTrack : AVCompositionTrack {
  func insertTimeRange(_ timeRange: CMTimeRange, of track: AVAssetTrack, at startTime: CMTime) throws
  @available(tvOS 5.0, *)
  func insertTimeRanges(_ timeRanges: [NSValue], of tracks: [AVAssetTrack], at startTime: CMTime) throws
  func insertEmptyTimeRange(_ timeRange: CMTimeRange)
  func removeTimeRange(_ timeRange: CMTimeRange)
  func scaleTimeRange(_ timeRange: CMTimeRange, toDuration duration: CMTime)
  func validateSegments(_ trackSegments: [AVCompositionTrackSegment]) throws
}

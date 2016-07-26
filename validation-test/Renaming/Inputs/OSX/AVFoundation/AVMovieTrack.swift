
@available(OSX 10.10, *)
class AVMovieTrack : AVAssetTrack {
  @available(OSX 10.11, *)
  var mediaPresentationTimeRange: CMTimeRange { get }
  @available(OSX 10.11, *)
  var mediaDecodeTimeRange: CMTimeRange { get }
  @available(OSX 10.11, *)
  var alternateGroupID: Int { get }
}
extension AVMovieTrack {
  @available(OSX 10.11, *)
  @NSCopying var mediaDataStorage: AVMediaDataStorage? { get }
}
@available(OSX 10.11, *)
class AVMutableMovieTrack : AVMovieTrack {
  @NSCopying var sampleReferenceBaseURL: NSURL?
  var isModified: Bool
  var hasProtectedContent: Bool { get }
  var timescale: CMTimeScale
}
extension AVMutableMovieTrack {
}
extension AVMutableMovieTrack {
  var layer: Int
  var cleanApertureDimensions: CGSize
  var productionApertureDimensions: CGSize
  var encodedPixelsDimensions: CGSize
}
extension AVMutableMovieTrack {
}
extension AVMutableMovieTrack {
  var preferredMediaChunkSize: Int
  var preferredMediaChunkDuration: CMTime
  var preferredMediaChunkAlignment: Int
}
extension AVMutableMovieTrack {
  func insertTimeRange(_ timeRange: CMTimeRange, of track: AVAssetTrack, at startTime: CMTime, copySampleData copySampleData: Bool) throws
  func insertEmptyTimeRange(_ timeRange: CMTimeRange)
  func removeTimeRange(_ timeRange: CMTimeRange)
  func scaleTimeRange(_ timeRange: CMTimeRange, toDuration duration: CMTime)
}
extension AVMutableMovieTrack {
}
extension AVMutableMovieTrack {
  func addTrackAssociation(to movieTrack: AVMovieTrack, type trackAssociationType: String)
  func removeTrackAssociation(to movieTrack: AVMovieTrack, type trackAssociationType: String)
}
@available(OSX 10.10, *)
let AVFragmentedMovieTrackTimeRangeDidChangeNotification: String
@available(OSX 10.10, *)
let AVFragmentedMovieTrackSegmentsDidChangeNotification: String
@available(OSX, introduced: 10.10, deprecated: 10.11, message: "Upon receipt of either AVFragmentedMovieTrackTimeRangeDidChangeNotification or AVFragmentedMovieTrackSegmentsDidChangeNotification, you can assume that the sender's totalSampleDataLength has changed.")
let AVFragmentedMovieTrackTotalSampleDataLengthDidChangeNotification: String
@available(OSX 10.10, *)
class AVFragmentedMovieTrack : AVMovieTrack {
}

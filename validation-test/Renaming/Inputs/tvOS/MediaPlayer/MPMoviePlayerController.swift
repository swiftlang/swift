
extension MPMoviePlayerController {
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var movieMediaTypes: MPMovieMediaTypeMask { get }
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var movieSourceType: MPMovieSourceType
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var duration: NSTimeInterval { get }
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var playableDuration: NSTimeInterval { get }
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var naturalSize: CGSize { get }
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var initialPlaybackTime: NSTimeInterval
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var endPlaybackTime: NSTimeInterval
  @available(tvOS, introduced: 4.3, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var allowsAirPlay: Bool
  @available(tvOS, introduced: 5.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var isAirPlayVideoActive: Bool { get }
}
@available(tvOS, introduced: 2.0, deprecated: 9.0)
let MPMoviePlayerScalingModeDidChangeNotification: String
@available(tvOS, introduced: 2.0, deprecated: 9.0)
let MPMoviePlayerPlaybackDidFinishNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerPlaybackDidFinishReasonUserInfoKey: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerPlaybackStateDidChangeNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerLoadStateDidChangeNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerNowPlayingMovieDidChangeNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerWillEnterFullscreenNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerDidEnterFullscreenNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerWillExitFullscreenNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerDidExitFullscreenNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerFullscreenAnimationDurationUserInfoKey: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerFullscreenAnimationCurveUserInfoKey: String
@available(tvOS, introduced: 5.0, deprecated: 9.0)
let MPMoviePlayerIsAirPlayVideoActiveDidChangeNotification: String
@available(tvOS, introduced: 6.0, deprecated: 9.0)
let MPMoviePlayerReadyForDisplayDidChangeNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMovieMediaTypesAvailableNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMovieSourceTypeAvailableNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMovieDurationAvailableNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMovieNaturalSizeAvailableNotification: String
extension MPMoviePlayerController {
  @available(tvOS, introduced: 3.2, deprecated: 9.0)
  func requestThumbnailImages(atTimes playbackTimes: [AnyObject]!, timeOption option: MPMovieTimeOption)
  @available(tvOS, introduced: 3.2, deprecated: 9.0)
  func cancelAllThumbnailImageRequests()
}
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailImageRequestDidFinishNotification: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailImageKey: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailTimeKey: String
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailErrorKey: String
extension MPMoviePlayerController {
  @available(tvOS, introduced: 4.0, deprecated: 9.0)
  var timedMetadata: [AnyObject]! { get }
}
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataUpdatedNotification: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataUserInfoKey: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyName: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyInfo: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyMIMEType: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyDataType: String
@available(tvOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyLanguageCode: String
extension MPMoviePlayerController {
  @available(tvOS, introduced: 4.3, deprecated: 9.0)
  var accessLog: MPMovieAccessLog! { get }
  @available(tvOS, introduced: 4.3, deprecated: 9.0)
  var errorLog: MPMovieErrorLog! { get }
}
extension MPMoviePlayerController {
}

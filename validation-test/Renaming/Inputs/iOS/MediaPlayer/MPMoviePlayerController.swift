
@available(iOS, introduced: 2.0, deprecated: 9.0)
enum MPMovieScalingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case aspectFit
  case aspectFill
  case fill
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMoviePlaybackState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stopped
  case playing
  case paused
  case interrupted
  case seekingForward
  case seekingBackward
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
struct MPMovieLoadState : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var playable: MPMovieLoadState { get }
  static var playthroughOK: MPMovieLoadState { get }
  static var stalled: MPMovieLoadState { get }
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMovieRepeatMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case one
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMovieControlStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case embedded
  case fullscreen
  static var `default`: MPMovieControlStyle { get }
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMovieFinishReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case playbackEnded
  case playbackError
  case userExited
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
struct MPMovieMediaTypeMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var video: MPMovieMediaTypeMask { get }
  static var audio: MPMovieMediaTypeMask { get }
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMovieSourceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case file
  case streaming
}
@available(iOS 2.0, *)
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
class MPMoviePlayerController : NSObject, MPMediaPlayback {
  init!(contentURL url: NSURL!)
  @NSCopying var contentURL: NSURL!
  var view: UIView! { get }
  var backgroundView: UIView! { get }
  var playbackState: MPMoviePlaybackState { get }
  var loadState: MPMovieLoadState { get }
  var controlStyle: MPMovieControlStyle
  var repeatMode: MPMovieRepeatMode
  var shouldAutoplay: Bool
  var isFullscreen: Bool
  func setFullscreen(_ fullscreen: Bool, animated animated: Bool)
  var scalingMode: MPMovieScalingMode
  @available(iOS 6.0, *)
  var readyForDisplay: Bool { get }
}
extension MPMoviePlayerController {
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var movieMediaTypes: MPMovieMediaTypeMask { get }
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var movieSourceType: MPMovieSourceType
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var duration: NSTimeInterval { get }
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var playableDuration: NSTimeInterval { get }
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var naturalSize: CGSize { get }
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var initialPlaybackTime: NSTimeInterval
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var endPlaybackTime: NSTimeInterval
  @available(iOS, introduced: 4.3, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var allowsAirPlay: Bool
  @available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  var isAirPlayVideoActive: Bool { get }
}
@available(iOS, introduced: 2.0, deprecated: 9.0)
let MPMoviePlayerScalingModeDidChangeNotification: String
@available(iOS, introduced: 2.0, deprecated: 9.0)
let MPMoviePlayerPlaybackDidFinishNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerPlaybackDidFinishReasonUserInfoKey: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerPlaybackStateDidChangeNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerLoadStateDidChangeNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerNowPlayingMovieDidChangeNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerWillEnterFullscreenNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerDidEnterFullscreenNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerWillExitFullscreenNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerDidExitFullscreenNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerFullscreenAnimationDurationUserInfoKey: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerFullscreenAnimationCurveUserInfoKey: String
@available(iOS, introduced: 5.0, deprecated: 9.0)
let MPMoviePlayerIsAirPlayVideoActiveDidChangeNotification: String
@available(iOS, introduced: 6.0, deprecated: 9.0)
let MPMoviePlayerReadyForDisplayDidChangeNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMovieMediaTypesAvailableNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMovieSourceTypeAvailableNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMovieDurationAvailableNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMovieNaturalSizeAvailableNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
enum MPMovieTimeOption : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case nearestKeyFrame
  case exact
}
extension MPMoviePlayerController {
  @available(iOS, introduced: 3.2, deprecated: 9.0)
  func requestThumbnailImages(atTimes playbackTimes: [AnyObject]!, timeOption option: MPMovieTimeOption)
  @available(iOS, introduced: 3.2, deprecated: 9.0)
  func cancelAllThumbnailImageRequests()
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailImageRequestDidFinishNotification: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailImageKey: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailTimeKey: String
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMoviePlayerThumbnailErrorKey: String
extension MPMoviePlayerController {
  @available(iOS, introduced: 4.0, deprecated: 9.0)
  var timedMetadata: [AnyObject]! { get }
}
@available(iOS 4.0, *)
@available(iOS, introduced: 4.0, deprecated: 9.0)
class MPTimedMetadata : NSObject {
  var key: String! { get }
  var keyspace: String! { get }
  var value: AnyObject! { get }
  var timestamp: NSTimeInterval { get }
  var allMetadata: [NSObject : AnyObject]! { get }
}
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataUpdatedNotification: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataUserInfoKey: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyName: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyInfo: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyMIMEType: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyDataType: String
@available(iOS, introduced: 4.0, deprecated: 9.0)
let MPMoviePlayerTimedMetadataKeyLanguageCode: String
extension MPMoviePlayerController {
  @available(iOS, introduced: 4.3, deprecated: 9.0)
  var accessLog: MPMovieAccessLog! { get }
  @available(iOS, introduced: 4.3, deprecated: 9.0)
  var errorLog: MPMovieErrorLog! { get }
}
@available(iOS 4.3, *)
@available(iOS, introduced: 4.3, deprecated: 9.0)
class MPMovieAccessLog : NSObject, NSCopying {
  var extendedLogData: NSData! { get }
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AnyObject]! { get }
}
@available(iOS 4.3, *)
@available(iOS, introduced: 4.3, deprecated: 9.0)
class MPMovieErrorLog : NSObject, NSCopying {
  var extendedLogData: NSData! { get }
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AnyObject]! { get }
}
@available(iOS 4.3, *)
@available(iOS, introduced: 4.3, deprecated: 9.0)
class MPMovieAccessLogEvent : NSObject, NSCopying {
  var numberOfSegmentsDownloaded: Int { get }
  var playbackStartDate: NSDate! { get }
  var uri: String! { get }
  var serverAddress: String! { get }
  var numberOfServerAddressChanges: Int { get }
  var playbackSessionID: String! { get }
  var playbackStartOffset: NSTimeInterval { get }
  var segmentsDownloadedDuration: NSTimeInterval { get }
  var durationWatched: NSTimeInterval { get }
  var numberOfStalls: Int { get }
  var numberOfBytesTransferred: Int64 { get }
  var observedBitrate: Double { get }
  var indicatedBitrate: Double { get }
  var numberOfDroppedVideoFrames: Int { get }
}
@available(iOS 4.3, *)
@available(iOS, introduced: 4.3, deprecated: 9.0)
class MPMovieErrorLogEvent : NSObject, NSCopying {
  var date: NSDate! { get }
  var uri: String! { get }
  var serverAddress: String! { get }
  var playbackSessionID: String! { get }
  var errorStatusCode: Int { get }
  var errorDomain: String! { get }
  var errorComment: String! { get }
}
extension MPMoviePlayerController {
}

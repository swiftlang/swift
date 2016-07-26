
@available(tvOS 5.0, *)
let AVPlayerItemTimeJumpedNotification: String
@available(tvOS 4.0, *)
let AVPlayerItemDidPlayToEndTimeNotification: String
@available(tvOS 4.3, *)
let AVPlayerItemFailedToPlayToEndTimeNotification: String
@available(tvOS 6.0, *)
let AVPlayerItemPlaybackStalledNotification: String
@available(tvOS 6.0, *)
let AVPlayerItemNewAccessLogEntryNotification: String
@available(tvOS 6.0, *)
let AVPlayerItemNewErrorLogEntryNotification: String
@available(tvOS 4.3, *)
let AVPlayerItemFailedToPlayToEndTimeErrorKey: String
enum AVPlayerItemStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case readyToPlay
  case failed
}
@available(tvOS 4.0, *)
class AVPlayerItem : NSObject, NSCopying {
  convenience init(url URL: NSURL)
  convenience init(asset asset: AVAsset)
  @available(tvOS 7.0, *)
  init(asset asset: AVAsset, automaticallyLoadedAssetKeys automaticallyLoadedAssetKeys: [String]?)
  var status: AVPlayerItemStatus { get }
  var error: NSError? { get }
}
extension AVPlayerItem {
  var asset: AVAsset { get }
  var tracks: [AVPlayerItemTrack] { get }
  @available(tvOS 4.3, *)
  var duration: CMTime { get }
  var presentationSize: CGSize { get }
  var timedMetadata: [AVMetadataItem]? { get }
  @available(tvOS 7.0, *)
  var automaticallyLoadedAssetKeys: [String] { get }
}
extension AVPlayerItem {
  @available(tvOS 5.0, *)
  var canPlayFastForward: Bool { get }
  @available(tvOS 6.0, *)
  var canPlaySlowForward: Bool { get }
  @available(tvOS 6.0, *)
  var canPlayReverse: Bool { get }
  @available(tvOS 6.0, *)
  var canPlaySlowReverse: Bool { get }
  @available(tvOS 5.0, *)
  var canPlayFastReverse: Bool { get }
  @available(tvOS 6.0, *)
  var canStepForward: Bool { get }
  @available(tvOS 6.0, *)
  var canStepBackward: Bool { get }
}
extension AVPlayerItem {
  @discardableResult
  func currentTime() -> CMTime
  var forwardPlaybackEndTime: CMTime
  var reversePlaybackEndTime: CMTime
  var seekableTimeRanges: [NSValue] { get }
  func seek(to time: CMTime)
  @available(tvOS 5.0, *)
  func seek(to time: CMTime, completionHandler completionHandler: (Bool) -> Void)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime)
  @available(tvOS 5.0, *)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime, completionHandler completionHandler: (Bool) -> Void)
  @available(tvOS 5.0, *)
  func cancelPendingSeeks()
  @discardableResult
  func currentDate() -> NSDate?
  @discardableResult
  func seek(to date: NSDate) -> Bool
  @available(tvOS 6.0, *)
  @discardableResult
  func seek(to date: NSDate, completionHandler completionHandler: (Bool) -> Void) -> Bool
  func step(byCount stepCount: Int)
  @available(tvOS 6.0, *)
  var timebase: CMTimebase? { get }
}
extension AVPlayerItem {
  @NSCopying var videoComposition: AVVideoComposition?
  @available(tvOS 7.0, *)
  var customVideoCompositor: AVVideoCompositing? { get }
  @available(tvOS 6.0, *)
  var seekingWaitsForVideoCompositionRendering: Bool
  @available(tvOS 6.0, *)
  var textStyleRules: [AVTextStyleRule]?
}
extension AVPlayerItem {
  @available(tvOS 7.0, *)
  var audioTimePitchAlgorithm: String
  @NSCopying var audioMix: AVAudioMix?
}
extension AVPlayerItem {
  var loadedTimeRanges: [NSValue] { get }
  var isPlaybackLikelyToKeepUp: Bool { get }
  var isPlaybackBufferFull: Bool { get }
  var isPlaybackBufferEmpty: Bool { get }
  @available(tvOS 9.0, *)
  var canUseNetworkResourcesForLiveStreamingWhilePaused: Bool
}
extension AVPlayerItem {
  @available(tvOS 8.0, *)
  var preferredPeakBitRate: Double
}
extension AVPlayerItem {
  @available(tvOS 5.0, *)
  func select(_ mediaSelectionOption: AVMediaSelectionOption?, in mediaSelectionGroup: AVMediaSelectionGroup)
  @available(tvOS 7.0, *)
  func selectMediaOptionAutomatically(in mediaSelectionGroup: AVMediaSelectionGroup)
  @available(tvOS 5.0, *)
  @discardableResult
  func selectedMediaOption(in mediaSelectionGroup: AVMediaSelectionGroup) -> AVMediaSelectionOption?
  @available(tvOS 9.0, *)
  var currentMediaSelection: AVMediaSelection { get }
}
extension AVPlayerItem {
  @available(tvOS 4.3, *)
  @discardableResult
  func accessLog() -> AVPlayerItemAccessLog?
  @available(tvOS 4.3, *)
  @discardableResult
  func errorLog() -> AVPlayerItemErrorLog?
}
extension AVPlayerItem {
  @available(tvOS 6.0, *)
  func addOutput(_ output: AVPlayerItemOutput)
  @available(tvOS 6.0, *)
  func removeOutput(_ output: AVPlayerItemOutput)
  @available(tvOS 6.0, *)
  var outputs: [AVPlayerItemOutput] { get }
}
extension AVPlayerItem {
  @available(tvOS 9.3, *)
  func addMediaDataCollector(_ collector: AVPlayerItemMediaDataCollector)
  @available(tvOS 9.3, *)
  func removeMediaDataCollector(_ collector: AVPlayerItemMediaDataCollector)
  @available(tvOS 9.3, *)
  var mediaDataCollectors: [AVPlayerItemMediaDataCollector] { get }
}
@available(tvOS 4.3, *)
class AVPlayerItemAccessLog : NSObject, NSCopying {
  @discardableResult
  func extendedLogData() -> NSData?
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AVPlayerItemAccessLogEvent] { get }
}
@available(tvOS 4.3, *)
class AVPlayerItemErrorLog : NSObject, NSCopying {
  @discardableResult
  func extendedLogData() -> NSData?
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AVPlayerItemErrorLogEvent] { get }
}
@available(tvOS 4.3, *)
class AVPlayerItemAccessLogEvent : NSObject, NSCopying {
  @available(tvOS 6.0, *)
  var numberOfMediaRequests: Int { get }
  var playbackStartDate: NSDate? { get }
  var uri: String? { get }
  var serverAddress: String? { get }
  var numberOfServerAddressChanges: Int { get }
  var playbackSessionID: String? { get }
  var playbackStartOffset: NSTimeInterval { get }
  var segmentsDownloadedDuration: NSTimeInterval { get }
  var durationWatched: NSTimeInterval { get }
  var numberOfStalls: Int { get }
  var numberOfBytesTransferred: Int64 { get }
  @available(tvOS 7.0, *)
  var transferDuration: NSTimeInterval { get }
  var observedBitrate: Double { get }
  var indicatedBitrate: Double { get }
  var numberOfDroppedVideoFrames: Int { get }
  @available(tvOS 7.0, *)
  var startupTime: NSTimeInterval { get }
  @available(tvOS 7.0, *)
  var downloadOverdue: Int { get }
  @available(tvOS 7.0, *)
  var observedMaxBitrate: Double { get }
  @available(tvOS 7.0, *)
  var observedMinBitrate: Double { get }
  @available(tvOS 7.0, *)
  var observedBitrateStandardDeviation: Double { get }
  @available(tvOS 7.0, *)
  var playbackType: String? { get }
  @available(tvOS 7.0, *)
  var mediaRequestsWWAN: Int { get }
  @available(tvOS 7.0, *)
  var switchBitrate: Double { get }
}
@available(tvOS 4.3, *)
class AVPlayerItemErrorLogEvent : NSObject, NSCopying {
  var date: NSDate? { get }
  var uri: String? { get }
  var serverAddress: String? { get }
  var playbackSessionID: String? { get }
  var errorStatusCode: Int { get }
  var errorDomain: String { get }
  var errorComment: String? { get }
}

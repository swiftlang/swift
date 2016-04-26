
@available(OSX 10.7, *)
let AVPlayerItemTimeJumpedNotification: String
@available(OSX 10.7, *)
let AVPlayerItemDidPlayToEndTimeNotification: String
@available(OSX 10.7, *)
let AVPlayerItemFailedToPlayToEndTimeNotification: String
@available(OSX 10.9, *)
let AVPlayerItemPlaybackStalledNotification: String
@available(OSX 10.9, *)
let AVPlayerItemNewAccessLogEntryNotification: String
@available(OSX 10.9, *)
let AVPlayerItemNewErrorLogEntryNotification: String
@available(OSX 10.7, *)
let AVPlayerItemFailedToPlayToEndTimeErrorKey: String
enum AVPlayerItemStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case readyToPlay
  case failed
}
@available(OSX 10.7, *)
class AVPlayerItem : NSObject, NSCopying {
  convenience init(url URL: NSURL)
  convenience init(asset asset: AVAsset)
  @available(OSX 10.9, *)
  init(asset asset: AVAsset, automaticallyLoadedAssetKeys automaticallyLoadedAssetKeys: [String]?)
  var status: AVPlayerItemStatus { get }
  var error: NSError? { get }
}
extension AVPlayerItem {
  var asset: AVAsset { get }
  var tracks: [AVPlayerItemTrack] { get }
  @available(OSX 10.7, *)
  var duration: CMTime { get }
  var presentationSize: CGSize { get }
  var timedMetadata: [AVMetadataItem]? { get }
  @available(OSX 10.9, *)
  var automaticallyLoadedAssetKeys: [String] { get }
}
extension AVPlayerItem {
  @available(OSX 10.8, *)
  var canPlayFastForward: Bool { get }
  @available(OSX 10.8, *)
  var canPlaySlowForward: Bool { get }
  @available(OSX 10.8, *)
  var canPlayReverse: Bool { get }
  @available(OSX 10.8, *)
  var canPlaySlowReverse: Bool { get }
  @available(OSX 10.8, *)
  var canPlayFastReverse: Bool { get }
  @available(OSX 10.8, *)
  var canStepForward: Bool { get }
  @available(OSX 10.8, *)
  var canStepBackward: Bool { get }
}
extension AVPlayerItem {
  @discardableResult
  func currentTime() -> CMTime
  var forwardPlaybackEndTime: CMTime
  var reversePlaybackEndTime: CMTime
  var seekableTimeRanges: [NSValue] { get }
  func seek(to time: CMTime)
  @available(OSX 10.7, *)
  func seek(to time: CMTime, completionHandler completionHandler: (Bool) -> Void)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime)
  @available(OSX 10.7, *)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime, completionHandler completionHandler: (Bool) -> Void)
  @available(OSX 10.7, *)
  func cancelPendingSeeks()
  @discardableResult
  func currentDate() -> NSDate?
  @discardableResult
  func seek(to date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func seek(to date: NSDate, completionHandler completionHandler: (Bool) -> Void) -> Bool
  func step(byCount stepCount: Int)
  @available(OSX 10.8, *)
  var timebase: CMTimebase? { get }
}
extension AVPlayerItem {
  @NSCopying var videoComposition: AVVideoComposition?
  @available(OSX 10.9, *)
  var customVideoCompositor: AVVideoCompositing? { get }
  @available(OSX 10.9, *)
  var seekingWaitsForVideoCompositionRendering: Bool
  @available(OSX 10.9, *)
  var textStyleRules: [AVTextStyleRule]?
}
extension AVPlayerItem {
  @available(OSX 10.9, *)
  var audioTimePitchAlgorithm: String
  @NSCopying var audioMix: AVAudioMix?
}
extension AVPlayerItem {
  var loadedTimeRanges: [NSValue] { get }
  var isPlaybackLikelyToKeepUp: Bool { get }
  var isPlaybackBufferFull: Bool { get }
  var isPlaybackBufferEmpty: Bool { get }
  @available(OSX 10.11, *)
  var canUseNetworkResourcesForLiveStreamingWhilePaused: Bool
}
extension AVPlayerItem {
  @available(OSX 10.10, *)
  var preferredPeakBitRate: Double
}
extension AVPlayerItem {
  @available(OSX 10.8, *)
  func select(_ mediaSelectionOption: AVMediaSelectionOption?, in mediaSelectionGroup: AVMediaSelectionGroup)
  @available(OSX 10.9, *)
  func selectMediaOptionAutomatically(in mediaSelectionGroup: AVMediaSelectionGroup)
  @available(OSX 10.8, *)
  @discardableResult
  func selectedMediaOption(in mediaSelectionGroup: AVMediaSelectionGroup) -> AVMediaSelectionOption?
  @available(OSX 10.11, *)
  var currentMediaSelection: AVMediaSelection { get }
}
extension AVPlayerItem {
  @available(OSX 10.7, *)
  @discardableResult
  func accessLog() -> AVPlayerItemAccessLog?
  @available(OSX 10.7, *)
  @discardableResult
  func errorLog() -> AVPlayerItemErrorLog?
}
extension AVPlayerItem {
  @available(OSX 10.8, *)
  func addOutput(_ output: AVPlayerItemOutput)
  @available(OSX 10.8, *)
  func removeOutput(_ output: AVPlayerItemOutput)
  @available(OSX 10.8, *)
  var outputs: [AVPlayerItemOutput] { get }
}
@available(OSX 10.7, *)
class AVPlayerItemAccessLog : NSObject, NSCopying {
  @discardableResult
  func extendedLogData() -> NSData?
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AVPlayerItemAccessLogEvent] { get }
}
@available(OSX 10.7, *)
class AVPlayerItemErrorLog : NSObject, NSCopying {
  @discardableResult
  func extendedLogData() -> NSData?
  var extendedLogDataStringEncoding: UInt { get }
  var events: [AVPlayerItemErrorLogEvent] { get }
}
@available(OSX 10.7, *)
class AVPlayerItemAccessLogEvent : NSObject, NSCopying {
  @available(OSX 10.9, *)
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
  @available(OSX 10.9, *)
  var transferDuration: NSTimeInterval { get }
  var observedBitrate: Double { get }
  var indicatedBitrate: Double { get }
  var numberOfDroppedVideoFrames: Int { get }
  @available(OSX 10.9, *)
  var startupTime: NSTimeInterval { get }
  @available(OSX 10.9, *)
  var downloadOverdue: Int { get }
  @available(OSX 10.9, *)
  var observedMaxBitrate: Double { get }
  @available(OSX 10.9, *)
  var observedMinBitrate: Double { get }
  @available(OSX 10.9, *)
  var observedBitrateStandardDeviation: Double { get }
  @available(OSX 10.9, *)
  var playbackType: String? { get }
  @available(OSX 10.9, *)
  var mediaRequestsWWAN: Int { get }
  @available(OSX 10.9, *)
  var switchBitrate: Double { get }
}
@available(OSX 10.7, *)
class AVPlayerItemErrorLogEvent : NSObject, NSCopying {
  var date: NSDate? { get }
  var uri: String? { get }
  var serverAddress: String? { get }
  var playbackSessionID: String? { get }
  var errorStatusCode: Int { get }
  var errorDomain: String { get }
  var errorComment: String? { get }
}

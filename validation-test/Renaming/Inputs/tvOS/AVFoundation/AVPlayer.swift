
enum AVPlayerStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case readyToPlay
  case failed
}
@available(tvOS 4.0, *)
class AVPlayer : NSObject {
  init(url URL: NSURL)
  init(playerItem item: AVPlayerItem)
  var status: AVPlayerStatus { get }
  var error: NSError? { get }
}
extension AVPlayer {
  var rate: Float
  func play()
  func pause()
}
extension AVPlayer {
  var currentItem: AVPlayerItem? { get }
  func replaceCurrentItem(with item: AVPlayerItem?)
  var actionAtItemEnd: AVPlayerActionAtItemEnd
}
enum AVPlayerActionAtItemEnd : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case advance
  case pause
  case none
}
extension AVPlayer {
  @discardableResult
  func currentTime() -> CMTime
  func seek(to date: NSDate)
  @available(tvOS 5.0, *)
  func seek(to date: NSDate, completionHandler completionHandler: (Bool) -> Void)
  func seek(to time: CMTime)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime)
  @available(tvOS 5.0, *)
  func seek(to time: CMTime, completionHandler completionHandler: (Bool) -> Void)
  @available(tvOS 5.0, *)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime, completionHandler completionHandler: (Bool) -> Void)
}
extension AVPlayer {
  @available(tvOS 6.0, *)
  func setRate(_ rate: Float, time itemTime: CMTime, atHostTime hostClockTime: CMTime)
  @available(tvOS 6.0, *)
  func preroll(atRate rate: Float, completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(tvOS 6.0, *)
  func cancelPendingPrerolls()
  @available(tvOS 6.0, *)
  var masterClock: CMClock?
}
extension AVPlayer {
  @discardableResult
  func addPeriodicTimeObserver(forInterval interval: CMTime, queue queue: dispatch_queue_t?, using block: (CMTime) -> Void) -> AnyObject
  @discardableResult
  func addBoundaryTimeObserver(forTimes times: [NSValue], queue queue: dispatch_queue_t?, using block: () -> Void) -> AnyObject
  func removeTimeObserver(_ observer: AnyObject)
}
extension AVPlayer {
  @available(tvOS 7.0, *)
  var volume: Float
  @available(tvOS 7.0, *)
  var isMuted: Bool
  var isClosedCaptionDisplayEnabled: Bool
}
extension AVPlayer {
  @available(tvOS 7.0, *)
  var appliesMediaSelectionCriteriaAutomatically: Bool
  @available(tvOS 7.0, *)
  func setMediaSelectionCriteria(_ criteria: AVPlayerMediaSelectionCriteria?, forMediaCharacteristic mediaCharacteristic: String)
  @available(tvOS 7.0, *)
  @discardableResult
  func mediaSelectionCriteria(forMediaCharacteristic mediaCharacteristic: String) -> AVPlayerMediaSelectionCriteria?
}
extension AVPlayer {
}
extension AVPlayer {
  @available(tvOS 6.0, *)
  var allowsExternalPlayback: Bool
  @available(tvOS 6.0, *)
  var isExternalPlaybackActive: Bool { get }
  @available(tvOS 6.0, *)
  var usesExternalPlaybackWhileExternalScreenIsActive: Bool
  @available(tvOS 6.0, *)
  var externalPlaybackVideoGravity: String
}
extension AVPlayer {
}
extension AVPlayer {
  @available(tvOS 6.0, *)
  var outputObscuredDueToInsufficientExternalProtection: Bool { get }
}
@available(tvOS 4.1, *)
class AVQueuePlayer : AVPlayer {
  init(items items: [AVPlayerItem])
  @discardableResult
  func items() -> [AVPlayerItem]
  func advanceToNextItem()
  @discardableResult
  func canInsertItem(_ item: AVPlayerItem, after afterItem: AVPlayerItem?) -> Bool
  func insertItem(_ item: AVPlayerItem, after afterItem: AVPlayerItem?)
  func removeItem(_ item: AVPlayerItem)
  func removeAllItems()
}

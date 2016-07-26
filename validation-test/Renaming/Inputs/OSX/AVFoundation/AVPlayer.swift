
enum AVPlayerStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case readyToPlay
  case failed
}
@available(OSX 10.7, *)
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
  @available(OSX 10.7, *)
  func seek(to date: NSDate, completionHandler completionHandler: (Bool) -> Void)
  func seek(to time: CMTime)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime)
  @available(OSX 10.7, *)
  func seek(to time: CMTime, completionHandler completionHandler: (Bool) -> Void)
  @available(OSX 10.7, *)
  func seek(to time: CMTime, toleranceBefore toleranceBefore: CMTime, toleranceAfter toleranceAfter: CMTime, completionHandler completionHandler: (Bool) -> Void)
}
extension AVPlayer {
  @available(OSX 10.8, *)
  func setRate(_ rate: Float, time itemTime: CMTime, atHostTime hostClockTime: CMTime)
  @available(OSX 10.8, *)
  func preroll(atRate rate: Float, completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.8, *)
  func cancelPendingPrerolls()
  @available(OSX 10.8, *)
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
  @available(OSX 10.7, *)
  var volume: Float
  @available(OSX 10.7, *)
  var isMuted: Bool
  var isClosedCaptionDisplayEnabled: Bool
}
extension AVPlayer {
  @available(OSX 10.9, *)
  var appliesMediaSelectionCriteriaAutomatically: Bool
  @available(OSX 10.9, *)
  func setMediaSelectionCriteria(_ criteria: AVPlayerMediaSelectionCriteria?, forMediaCharacteristic mediaCharacteristic: String)
  @available(OSX 10.9, *)
  @discardableResult
  func mediaSelectionCriteria(forMediaCharacteristic mediaCharacteristic: String) -> AVPlayerMediaSelectionCriteria?
}
extension AVPlayer {
  @available(OSX 10.9, *)
  var audioOutputDeviceUniqueID: String?
}
extension AVPlayer {
  @available(OSX 10.11, *)
  var allowsExternalPlayback: Bool
  @available(OSX 10.11, *)
  var isExternalPlaybackActive: Bool { get }
}
@available(OSX 10.7, *)
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


@available(watchOS 2.0, *)
enum WKAudioFilePlayerStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case readyToPlay
  case failed
}
@available(watchOS 2.0, *)
class WKAudioFilePlayer : NSObject {
  convenience init(playerItem item: WKAudioFilePlayerItem)
  func play()
  func pause()
  func replaceCurrentItem(with item: WKAudioFilePlayerItem?)
  var currentItem: WKAudioFilePlayerItem? { get }
  var status: WKAudioFilePlayerStatus { get }
  var error: NSError? { get }
  var rate: Float
  var currentTime: NSTimeInterval { get }
}
@available(watchOS 2.0, *)
class WKAudioFileQueuePlayer : WKAudioFilePlayer {
  convenience init(items items: [WKAudioFilePlayerItem])
  func advanceToNextItem()
  func appendItem(_ item: WKAudioFilePlayerItem)
  func removeItem(_ item: WKAudioFilePlayerItem)
  func removeAllItems()
  var items: [WKAudioFilePlayerItem] { get }
}

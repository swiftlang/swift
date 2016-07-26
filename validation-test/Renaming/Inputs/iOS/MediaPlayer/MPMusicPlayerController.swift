
enum MPMusicPlaybackState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stopped
  case playing
  case paused
  case interrupted
  case seekingForward
  case seekingBackward
}
enum MPMusicRepeatMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case none
  case one
  case all
}
enum MPMusicShuffleMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case off
  case songs
  case albums
}
@available(iOS 3.0, *)
class MPMusicPlayerController : NSObject, MPMediaPlayback {
  @discardableResult
  class func applicationMusicPlayer() -> MPMusicPlayerController
  @discardableResult
  class func systemMusicPlayer() -> MPMusicPlayerController
  @available(iOS, introduced: 3.0, deprecated: 8.0, message: "Use +systemMusicPlayer instead.")
  @discardableResult
  class func iPodMusicPlayer() -> MPMusicPlayerController
}
extension MPMusicPlayerController {
  var playbackState: MPMusicPlaybackState { get }
  var repeatMode: MPMusicRepeatMode
  var shuffleMode: MPMusicShuffleMode
  @NSCopying var nowPlayingItem: MPMediaItem?
  @available(iOS 5.0, *)
  var indexOfNowPlayingItem: Int { get }
  func setQueueWith(_ query: MPMediaQuery)
  func setQueueWith(_ itemCollection: MPMediaItemCollection)
  @available(iOS 9.3, *)
  func setQueueWithStoreIDs(_ storeIDs: [String])
  func skipToNextItem()
  func skipToBeginning()
  func skipToPreviousItem()
  func beginGeneratingPlaybackNotifications()
  func endGeneratingPlaybackNotifications()
}
let MPMusicPlayerControllerPlaybackStateDidChangeNotification: String
let MPMusicPlayerControllerNowPlayingItemDidChangeNotification: String
let MPMusicPlayerControllerVolumeDidChangeNotification: String

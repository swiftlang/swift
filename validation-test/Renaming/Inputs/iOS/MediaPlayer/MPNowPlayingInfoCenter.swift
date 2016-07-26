
@available(iOS 5.0, *)
class MPNowPlayingInfoCenter : NSObject {
  @discardableResult
  class func defaultCenter() -> MPNowPlayingInfoCenter
  var nowPlayingInfo: [String : AnyObject]?
}
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyElapsedPlaybackTime: String
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackRate: String
@available(iOS 8.0, *)
let MPNowPlayingInfoPropertyDefaultPlaybackRate: String
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackQueueIndex: String
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackQueueCount: String
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyChapterNumber: String
@available(iOS 5.0, *)
let MPNowPlayingInfoPropertyChapterCount: String
@available(iOS 9.0, *)
let MPNowPlayingInfoPropertyAvailableLanguageOptions: String
@available(iOS 9.0, *)
let MPNowPlayingInfoPropertyCurrentLanguageOptions: String

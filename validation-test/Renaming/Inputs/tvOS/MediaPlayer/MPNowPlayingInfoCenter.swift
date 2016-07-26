
@available(tvOS 5.0, *)
class MPNowPlayingInfoCenter : NSObject {
  @discardableResult
  class func defaultCenter() -> MPNowPlayingInfoCenter
  var nowPlayingInfo: [String : AnyObject]?
}
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyElapsedPlaybackTime: String
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackRate: String
@available(tvOS 8.0, *)
let MPNowPlayingInfoPropertyDefaultPlaybackRate: String
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackQueueIndex: String
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyPlaybackQueueCount: String
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyChapterNumber: String
@available(tvOS 5.0, *)
let MPNowPlayingInfoPropertyChapterCount: String
@available(tvOS 9.0, *)
let MPNowPlayingInfoPropertyAvailableLanguageOptions: String
@available(tvOS 9.0, *)
let MPNowPlayingInfoPropertyCurrentLanguageOptions: String

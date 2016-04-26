
@available(tvOS 7.1, *)
class MPRemoteCommandCenter : NSObject {
  var pauseCommand: MPRemoteCommand { get }
  var playCommand: MPRemoteCommand { get }
  var stopCommand: MPRemoteCommand { get }
  var togglePlayPauseCommand: MPRemoteCommand { get }
  var enableLanguageOptionCommand: MPRemoteCommand { get }
  var disableLanguageOptionCommand: MPRemoteCommand { get }
  var nextTrackCommand: MPRemoteCommand { get }
  var previousTrackCommand: MPRemoteCommand { get }
  var skipForwardCommand: MPSkipIntervalCommand { get }
  var skipBackwardCommand: MPSkipIntervalCommand { get }
  var seekForwardCommand: MPRemoteCommand { get }
  var seekBackwardCommand: MPRemoteCommand { get }
  var ratingCommand: MPRatingCommand { get }
  var changePlaybackRateCommand: MPChangePlaybackRateCommand { get }
  var likeCommand: MPFeedbackCommand { get }
  var dislikeCommand: MPFeedbackCommand { get }
  var bookmarkCommand: MPFeedbackCommand { get }
  var changePlaybackPositionCommand: MPChangePlaybackPositionCommand { get }
  @discardableResult
  class func shared() -> MPRemoteCommandCenter
}

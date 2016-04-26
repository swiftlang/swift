
@available(iOS 7.1, *)
enum MPRemoteCommandHandlerStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case success
  case noSuchContent
  @available(iOS 9.1, *)
  case noActionableNowPlayingItem
  case commandFailed
}
@available(iOS 7.1, *)
class MPRemoteCommand : NSObject {
  var isEnabled: Bool
  func addTarget(_ target: AnyObject, action action: Selector)
  func removeTarget(_ target: AnyObject, action action: Selector?)
  func removeTarget(_ target: AnyObject?)
  @discardableResult
  func addTarget(handler handler: (MPRemoteCommandEvent) -> MPRemoteCommandHandlerStatus) -> AnyObject
}
@available(iOS 7.1, *)
class MPSkipIntervalCommand : MPRemoteCommand {
  var preferredIntervals: [AnyObject]
}
@available(iOS 7.1, *)
class MPFeedbackCommand : MPRemoteCommand {
  var isActive: Bool
  var localizedTitle: String
  @available(iOS 8.0, *)
  var localizedShortTitle: String
}
@available(iOS 7.1, *)
class MPRatingCommand : MPRemoteCommand {
  var minimumRating: Float
  var maximumRating: Float
}
@available(iOS 7.1, *)
class MPChangePlaybackRateCommand : MPRemoteCommand {
  var supportedPlaybackRates: [NSNumber]
}
@available(iOS 9.0, *)
class MPChangePlaybackPositionCommand : MPRemoteCommand {
}

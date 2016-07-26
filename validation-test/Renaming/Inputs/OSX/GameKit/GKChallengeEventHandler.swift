
@available(OSX, introduced: 10.8, deprecated: 10.10, message: "You should instead implement the GKChallengeListener protocol and register a listener with GKLocalPlayer.")
protocol GKChallengeEventHandlerDelegate : NSObjectProtocol {
  optional func localPlayerDidSelect(_ challenge: GKChallenge!)
  @discardableResult
  optional func shouldShowBanner(forLocallyReceivedChallenge challenge: GKChallenge!) -> Bool
  optional func localPlayerDidReceive(_ challenge: GKChallenge!)
  @discardableResult
  optional func shouldShowBanner(forLocallyCompletedChallenge challenge: GKChallenge!) -> Bool
  optional func localPlayerDidComplete(_ challenge: GKChallenge!)
  @discardableResult
  optional func shouldShowBanner(forRemotelyCompletedChallenge challenge: GKChallenge!) -> Bool
  optional func remotePlayerDidComplete(_ challenge: GKChallenge!)
}
@available(OSX, introduced: 10.8, deprecated: 10.10, message: "You should instead implement the GKChallengeListener protocol and register a listener with GKLocalPlayer.")
class GKChallengeEventHandler : NSObject {
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  unowned(unsafe) var delegate: @sil_unmanaged GKChallengeEventHandlerDelegate!
}

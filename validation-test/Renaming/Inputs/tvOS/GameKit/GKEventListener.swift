
protocol GKChallengeListener : NSObjectProtocol {
  @available(tvOS 7.0, *)
  optional func player(_ player: GKPlayer, wantsToPlay challenge: GKChallenge)
  @available(tvOS 7.0, *)
  optional func player(_ player: GKPlayer, didReceive challenge: GKChallenge)
  @available(tvOS 7.0, *)
  optional func player(_ player: GKPlayer, didComplete challenge: GKChallenge, issuedByFriend friendPlayer: GKPlayer)
  @available(tvOS 7.0, *)
  optional func player(_ player: GKPlayer, issuedChallengeWasCompleted challenge: GKChallenge, byFriend friendPlayer: GKPlayer)
}

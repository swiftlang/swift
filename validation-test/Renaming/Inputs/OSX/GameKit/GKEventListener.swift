
protocol GKChallengeListener : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, wantsToPlay challenge: GKChallenge)
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, didReceive challenge: GKChallenge)
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, didComplete challenge: GKChallenge, issuedByFriend friendPlayer: GKPlayer)
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, issuedChallengeWasCompleted challenge: GKChallenge, byFriend friendPlayer: GKPlayer)
}

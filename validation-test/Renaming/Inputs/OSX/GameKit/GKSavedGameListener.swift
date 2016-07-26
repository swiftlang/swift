
@available(OSX 10.10, *)
protocol GKSavedGameListener : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, didModifySavedGame savedGame: GKSavedGame)
  @available(OSX 10.10, *)
  optional func player(_ player: GKPlayer, hasConflictingSavedGames savedGames: [GKSavedGame])
}

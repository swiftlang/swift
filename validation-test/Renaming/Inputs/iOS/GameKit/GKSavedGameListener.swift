
@available(iOS 8.0, *)
protocol GKSavedGameListener : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func player(_ player: GKPlayer, didModifySavedGame savedGame: GKSavedGame)
  @available(iOS 8.0, *)
  optional func player(_ player: GKPlayer, hasConflictingSavedGames savedGames: [GKSavedGame])
}

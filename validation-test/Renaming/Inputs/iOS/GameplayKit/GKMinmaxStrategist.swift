
@available(iOS 9.0, *)
class GKMinmaxStrategist : NSObject, GKStrategist {
  var maxLookAheadDepth: Int
  @discardableResult
  func bestMove(for player: GKGameModelPlayer) -> GKGameModelUpdate?
  @discardableResult
  func randomMove(for player: GKGameModelPlayer, fromNumberOfBestMoves numMovesToConsider: Int) -> GKGameModelUpdate?
}

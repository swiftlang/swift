
@available(OSX 10.11, *)
class GKState : NSObject {
  weak var stateMachine: @sil_weak GKStateMachine? { get }
  @discardableResult
  func isValidNextState(_ stateClass: AnyClass) -> Bool
  func didEnter(withPreviousState previousState: GKState?)
  func update(withDeltaTime seconds: NSTimeInterval)
  func willExit(withNextState nextState: GKState)
}

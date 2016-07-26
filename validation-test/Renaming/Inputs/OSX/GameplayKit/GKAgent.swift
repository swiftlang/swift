
protocol GKAgentDelegate : NSObjectProtocol {
  @available(OSX 10.11, *)
  optional func agentWillUpdate(_ agent: GKAgent)
  @available(OSX 10.11, *)
  optional func agentDidUpdate(_ agent: GKAgent)
}
@available(OSX 10.11, *)
class GKAgent : GKComponent {
  weak var delegate: @sil_weak GKAgentDelegate?
  var behavior: GKBehavior?
  var mass: Float
  var radius: Float
  var speed: Float { get }
  var maxAcceleration: Float
  var maxSpeed: Float
}
@available(OSX 10.11, *)
class GKAgent2D : GKAgent {
  var position: vector_float2
  var velocity: vector_float2 { get }
  var rotation: Float
}

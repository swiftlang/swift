
protocol GKAgentDelegate : NSObjectProtocol {
  @available(tvOS 9.0, *)
  optional func agentWillUpdate(_ agent: GKAgent)
  @available(tvOS 9.0, *)
  optional func agentDidUpdate(_ agent: GKAgent)
}
@available(tvOS 9.0, *)
class GKAgent : GKComponent {
  weak var delegate: @sil_weak GKAgentDelegate?
  var behavior: GKBehavior?
  var mass: Float
  var radius: Float
  var speed: Float { get }
  var maxAcceleration: Float
  var maxSpeed: Float
}
@available(tvOS 9.0, *)
class GKAgent2D : GKAgent {
  var position: vector_float2
  var velocity: vector_float2 { get }
  var rotation: Float
}

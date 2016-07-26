
@available(OSX 10.11, *)
class GKGoal : NSObject, NSCopying {
  convenience init(toSeekAgent agent: GKAgent)
  convenience init(toFleeAgent agent: GKAgent)
  convenience init(toAvoid obstacles: [GKObstacle], maxPredictionTime maxPredictionTime: NSTimeInterval)
  convenience init(toAvoid agents: [GKAgent], maxPredictionTime maxPredictionTime: NSTimeInterval)
  convenience init(toSeparateFrom agents: [GKAgent], maxDistance maxDistance: Float, maxAngle maxAngle: Float)
  convenience init(toAlignWith agents: [GKAgent], maxDistance maxDistance: Float, maxAngle maxAngle: Float)
  convenience init(toCohereWith agents: [GKAgent], maxDistance maxDistance: Float, maxAngle maxAngle: Float)
  convenience init(toReachTargetSpeed targetSpeed: Float)
  convenience init(toWander speed: Float)
  convenience init(toInterceptAgent target: GKAgent, maxPredictionTime maxPredictionTime: NSTimeInterval)
  convenience init(toFollow path: GKPath, maxPredictionTime maxPredictionTime: NSTimeInterval, forward forward: Bool)
  convenience init(toStayOn path: GKPath, maxPredictionTime maxPredictionTime: NSTimeInterval)
}

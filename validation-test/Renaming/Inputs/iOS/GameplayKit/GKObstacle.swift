
@available(iOS 9.0, *)
class GKObstacle : NSObject {
}
@available(iOS 9.0, *)
class GKCircleObstacle : GKObstacle {
  var radius: Float
  var position: vector_float2
  init(radius radius: Float)
}
@available(iOS 9.0, *)
class GKPolygonObstacle : GKObstacle {
  var vertexCount: Int { get }
  init(points points: UnsafeMutablePointer<vector_float2>, count numPoints: Int)
  @discardableResult
  func vertex(at index: Int) -> vector_float2
}

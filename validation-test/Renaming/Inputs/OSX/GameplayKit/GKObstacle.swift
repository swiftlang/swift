
@available(OSX 10.11, *)
class GKObstacle : NSObject {
}
@available(OSX 10.11, *)
class GKCircleObstacle : GKObstacle {
  var radius: Float
  var position: vector_float2
  init(radius radius: Float)
}
@available(OSX 10.11, *)
class GKPolygonObstacle : GKObstacle {
  var vertexCount: Int { get }
  init(points points: UnsafeMutablePointer<vector_float2>, count numPoints: Int)
  @discardableResult
  func vertex(at index: Int) -> vector_float2
}

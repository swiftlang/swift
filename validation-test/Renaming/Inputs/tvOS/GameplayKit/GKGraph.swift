
@available(tvOS 9.0, *)
class GKGraph : NSObject {
  var nodes: [GKGraphNode]? { get }
  init(nodes nodes: [GKGraphNode])
  func connectNode(toLowestCost node: GKGraphNode, bidirectional bidirectional: Bool)
  func removeNodes(_ nodes: [GKGraphNode])
  func addNodes(_ nodes: [GKGraphNode])
  @discardableResult
  func findPath(from startNode: GKGraphNode, to endNode: GKGraphNode) -> [GKGraphNode]
}
@available(tvOS 9.0, *)
class GKObstacleGraph : GKGraph {
  var obstacles: [GKPolygonObstacle] { get }
  var bufferRadius: Float { get }
  init(obstacles obstacles: [GKPolygonObstacle], bufferRadius bufferRadius: Float)
  func connectNode(usingObstacles node: GKGraphNode2D)
  func connectNode(usingObstacles node: GKGraphNode2D, ignoring obstaclesToIgnore: [GKPolygonObstacle])
  func connectNode(usingObstacles node: GKGraphNode2D, ignoringBufferRadiusOf obstaclesBufferRadiusToIgnore: [GKPolygonObstacle])
  func addObstacles(_ obstacles: [GKPolygonObstacle])
  func removeObstacles(_ obstacles: [GKPolygonObstacle])
  func removeAllObstacles()
  @discardableResult
  func nodes(for obstacle: GKPolygonObstacle) -> [GKGraphNode2D]
  func lockConnection(from startNode: GKGraphNode2D, to endNode: GKGraphNode2D)
  func unlockConnection(from startNode: GKGraphNode2D, to endNode: GKGraphNode2D)
  @discardableResult
  func isConnectionLocked(from startNode: GKGraphNode2D, to endNode: GKGraphNode2D) -> Bool
}
@available(tvOS 9.0, *)
class GKGridGraph : GKGraph {
  var gridOrigin: vector_int2 { get }
  var gridWidth: Int { get }
  var gridHeight: Int { get }
  var diagonalsAllowed: Bool { get }
  init(fromGridStartingAt position: vector_int2, width width: Int32, height height: Int32, diagonalsAllowed diagonalsAllowed: Bool)
  @discardableResult
  func node(atGridPosition position: vector_int2) -> GKGridGraphNode?
  func connectNode(toAdjacentNodes node: GKGridGraphNode)
}

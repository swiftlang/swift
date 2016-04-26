
@available(iOS 9.0, *)
class GKGraphNode : NSObject {
  var connectedNodes: [GKGraphNode] { get }
  func addConnections(to nodes: [GKGraphNode], bidirectional bidirectional: Bool)
  func removeConnections(to nodes: [GKGraphNode], bidirectional bidirectional: Bool)
  @discardableResult
  func estimatedCost(to node: GKGraphNode) -> Float
  @discardableResult
  func cost(to node: GKGraphNode) -> Float
  @discardableResult
  func findPath(to goalNode: GKGraphNode) -> [GKGraphNode]
  @discardableResult
  func findPath(from startNode: GKGraphNode) -> [GKGraphNode]
}
@available(iOS 9.0, *)
class GKGraphNode2D : GKGraphNode {
  var position: vector_float2
  @discardableResult
  class func node(withPoint point: vector_float2) -> Self
  init(point point: vector_float2)
}
@available(iOS 9.0, *)
class GKGridGraphNode : GKGraphNode {
  var gridPosition: vector_int2
  init(gridPosition gridPosition: vector_int2)
}

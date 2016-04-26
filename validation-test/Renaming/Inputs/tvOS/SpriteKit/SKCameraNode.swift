
@available(tvOS 9.0, *)
class SKCameraNode : SKNode {
  @discardableResult
  func contains(_ node: SKNode) -> Bool
  @discardableResult
  func containedNodeSet() -> Set<SKNode>
}

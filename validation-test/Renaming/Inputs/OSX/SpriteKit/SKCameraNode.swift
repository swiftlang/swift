
@available(OSX 10.11, *)
class SKCameraNode : SKNode {
  @discardableResult
  func contains(_ node: SKNode) -> Bool
  @discardableResult
  func containedNodeSet() -> Set<SKNode>
}

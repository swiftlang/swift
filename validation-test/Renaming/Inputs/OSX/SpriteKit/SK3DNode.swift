
@available(OSX 10.10, *)
class SK3DNode : SKNode {
  init(viewportSize viewportSize: CGSize)
  var viewportSize: CGSize
  var sceneTime: NSTimeInterval
  @discardableResult
  func projectPoint(_ point: vector_float3) -> vector_float3
  @discardableResult
  func unprojectPoint(_ point: vector_float3) -> vector_float3
  var isPlaying: Bool
  var loops: Bool
  var autoenablesDefaultLighting: Bool
}

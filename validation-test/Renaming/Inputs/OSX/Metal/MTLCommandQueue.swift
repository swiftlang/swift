
@available(OSX 10.11, *)
protocol MTLCommandQueue : NSObjectProtocol {
  var label: String? { get set }
  var device: MTLDevice { get }
  @discardableResult
  func commandBuffer() -> MTLCommandBuffer
  @discardableResult
  func commandBufferWithUnretainedReferences() -> MTLCommandBuffer
  func insertDebugCaptureBoundary()
}

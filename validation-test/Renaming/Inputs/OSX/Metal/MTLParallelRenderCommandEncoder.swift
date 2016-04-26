
@available(OSX 10.11, *)
protocol MTLParallelRenderCommandEncoder : MTLCommandEncoder {
  @discardableResult
  func renderCommandEncoder() -> MTLRenderCommandEncoder
}

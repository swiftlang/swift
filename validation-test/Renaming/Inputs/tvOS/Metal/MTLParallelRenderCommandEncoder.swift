
@available(tvOS 8.0, *)
protocol MTLParallelRenderCommandEncoder : MTLCommandEncoder {
  @discardableResult
  func renderCommandEncoder() -> MTLRenderCommandEncoder
}

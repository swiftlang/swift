
struct MTLDispatchThreadgroupsIndirectArguments {
  var threadgroupsPerGrid: (UInt32, UInt32, UInt32)
  init()
  init(threadgroupsPerGrid threadgroupsPerGrid: (UInt32, UInt32, UInt32))
}
@available(iOS 8.0, *)
protocol MTLComputeCommandEncoder : MTLCommandEncoder {
  func setComputePipelineState(_ state: MTLComputePipelineState)
  @available(iOS 8.3, *)
  func setBytes(_ bytes: UnsafePointer<Void>, length length: Int, at index: Int)
  func setBuffer(_ buffer: MTLBuffer?, offset offset: Int, at index: Int)
  @available(iOS 8.3, *)
  func setBufferOffset(_ offset: Int, at index: Int)
  func setBuffers(_ buffers: UnsafePointer<MTLBuffer?>!, offsets offsets: UnsafePointer<Int>!, with range: NSRange)
  func setTexture(_ texture: MTLTexture?, at index: Int)
  func setTextures(_ textures: UnsafePointer<MTLTexture?>!, with range: NSRange)
  func setSamplerState(_ sampler: MTLSamplerState?, at index: Int)
  func setSamplerStates(_ samplers: UnsafePointer<MTLSamplerState?>!, with range: NSRange)
  func setSamplerState(_ sampler: MTLSamplerState?, lodMinClamp lodMinClamp: Float, lodMaxClamp lodMaxClamp: Float, at index: Int)
  func setSamplerStates(_ samplers: UnsafePointer<MTLSamplerState?>!, lodMinClamps lodMinClamps: UnsafePointer<Float>!, lodMaxClamps lodMaxClamps: UnsafePointer<Float>!, with range: NSRange)
  func setThreadgroupMemoryLength(_ length: Int, at index: Int)
  func dispatchThreadgroups(_ threadgroupsPerGrid: MTLSize, threadsPerThreadgroup threadsPerThreadgroup: MTLSize)
  @available(iOS 9.0, *)
  func dispatchThreadgroups(withIndirectBuffer indirectBuffer: MTLBuffer, indirectBufferOffset indirectBufferOffset: Int, threadsPerThreadgroup threadsPerThreadgroup: MTLSize)
}

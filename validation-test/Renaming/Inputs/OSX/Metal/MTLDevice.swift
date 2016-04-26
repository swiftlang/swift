
@available(OSX 10.11, *)
@discardableResult
func MTLCreateSystemDefaultDevice() -> MTLDevice?
@available(OSX 10.11, *)
@discardableResult
func MTLCopyAllDevices() -> [MTLDevice]
@available(OSX 10.11, *)
enum MTLFeatureSet : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  @available(OSX 10.11, *)
  case osx_GPUFamily1_v1
}
@available(OSX 10.11, *)
struct MTLPipelineOption : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var argumentInfo: MTLPipelineOption { get }
  static var bufferTypeInfo: MTLPipelineOption { get }
}
typealias MTLAutoreleasedRenderPipelineReflection = MTLRenderPipelineReflection
typealias MTLAutoreleasedComputePipelineReflection = MTLComputePipelineReflection
typealias MTLNewLibraryCompletionHandler = (MTLLibrary?, NSError?) -> Void
typealias MTLNewRenderPipelineStateCompletionHandler = (MTLRenderPipelineState?, NSError?) -> Void
typealias MTLNewRenderPipelineStateWithReflectionCompletionHandler = (MTLRenderPipelineState?, MTLRenderPipelineReflection?, NSError?) -> Void
typealias MTLNewComputePipelineStateCompletionHandler = (MTLComputePipelineState?, NSError?) -> Void
typealias MTLNewComputePipelineStateWithReflectionCompletionHandler = (MTLComputePipelineState?, MTLComputePipelineReflection?, NSError?) -> Void
@available(OSX 10.11, *)
protocol MTLDevice : NSObjectProtocol {
  var name: String? { get }
  @available(OSX 10.11, *)
  var maxThreadsPerThreadgroup: MTLSize { get }
  @available(OSX 10.11, *)
  var isLowPower: Bool { get }
  @available(OSX 10.11, *)
  var isHeadless: Bool { get }
  @available(OSX 10.11, *)
  var isDepth24Stencil8PixelFormatSupported: Bool { get }
  @discardableResult
  func newCommandQueue() -> MTLCommandQueue
  @discardableResult
  func newCommandQueue(withMaxCommandBufferCount maxCommandBufferCount: Int) -> MTLCommandQueue
  @discardableResult
  func newBuffer(withLength length: Int, options options: MTLResourceOptions = []) -> MTLBuffer
  @discardableResult
  func newBuffer(withBytes pointer: UnsafePointer<Void>, length length: Int, options options: MTLResourceOptions = []) -> MTLBuffer
  @discardableResult
  func newBuffer(withBytesNoCopy pointer: UnsafeMutablePointer<Void>, length length: Int, options options: MTLResourceOptions = [], deallocator deallocator: ((UnsafeMutablePointer<Void>, Int) -> Void)? = nil) -> MTLBuffer
  @discardableResult
  func newDepthStencilState(with descriptor: MTLDepthStencilDescriptor) -> MTLDepthStencilState
  @discardableResult
  func newTexture(with descriptor: MTLTextureDescriptor) -> MTLTexture
  @available(OSX 10.11, *)
  @discardableResult
  func newTexture(with descriptor: MTLTextureDescriptor, iosurface iosurface: IOSurface, plane plane: Int) -> MTLTexture
  @discardableResult
  func newSamplerState(with descriptor: MTLSamplerDescriptor) -> MTLSamplerState
  @discardableResult
  func newDefaultLibrary() -> MTLLibrary?
  @discardableResult
  func newLibrary(withFile filepath: String) throws -> MTLLibrary
  @discardableResult
  func newLibrary(with data: dispatch_data_t) throws -> MTLLibrary
  @discardableResult
  func newLibrary(withSource source: String, options options: MTLCompileOptions?) throws -> MTLLibrary
  func newLibrary(withSource source: String, options options: MTLCompileOptions?, completionHandler completionHandler: MTLNewLibraryCompletionHandler)
  @discardableResult
  func newRenderPipelineState(with descriptor: MTLRenderPipelineDescriptor) throws -> MTLRenderPipelineState
  @discardableResult
  func newRenderPipelineState(with descriptor: MTLRenderPipelineDescriptor, options options: MTLPipelineOption, reflection reflection: AutoreleasingUnsafeMutablePointer<MTLAutoreleasedRenderPipelineReflection?>?) throws -> MTLRenderPipelineState
  func newRenderPipelineState(with descriptor: MTLRenderPipelineDescriptor, completionHandler completionHandler: MTLNewRenderPipelineStateCompletionHandler)
  func newRenderPipelineState(with descriptor: MTLRenderPipelineDescriptor, options options: MTLPipelineOption, completionHandler completionHandler: MTLNewRenderPipelineStateWithReflectionCompletionHandler)
  @discardableResult
  func newComputePipelineState(with computeFunction: MTLFunction) throws -> MTLComputePipelineState
  @discardableResult
  func newComputePipelineState(with computeFunction: MTLFunction, options options: MTLPipelineOption, reflection reflection: AutoreleasingUnsafeMutablePointer<MTLAutoreleasedComputePipelineReflection?>?) throws -> MTLComputePipelineState
  func newComputePipelineState(with computeFunction: MTLFunction, completionHandler completionHandler: MTLNewComputePipelineStateCompletionHandler)
  func newComputePipelineState(with computeFunction: MTLFunction, options options: MTLPipelineOption, completionHandler completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler)
  @available(OSX 10.11, *)
  @discardableResult
  func newComputePipelineState(with descriptor: MTLComputePipelineDescriptor, options options: MTLPipelineOption, reflection reflection: AutoreleasingUnsafeMutablePointer<MTLAutoreleasedComputePipelineReflection?>?) throws -> MTLComputePipelineState
  @available(OSX 10.11, *)
  func newComputePipelineState(with descriptor: MTLComputePipelineDescriptor, options options: MTLPipelineOption, completionHandler completionHandler: MTLNewComputePipelineStateWithReflectionCompletionHandler)
  @discardableResult
  func supportsFeatureSet(_ featureSet: MTLFeatureSet) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  func supportsTextureSampleCount(_ sampleCount: Int) -> Bool
}

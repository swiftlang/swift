
@available(OSX 10.11, *)
class MTLComputePipelineReflection : NSObject {
  var arguments: [MTLArgument] { get }
}
@available(OSX 10.11, *)
class MTLComputePipelineDescriptor : NSObject, NSCopying {
  var label: String?
  var computeFunction: MTLFunction?
  var threadGroupSizeIsMultipleOfThreadExecutionWidth: Bool
  func reset()
}
@available(OSX 10.11, *)
protocol MTLComputePipelineState : NSObjectProtocol {
  var device: MTLDevice { get }
  var maxTotalThreadsPerThreadgroup: Int { get }
  var threadExecutionWidth: Int { get }
}

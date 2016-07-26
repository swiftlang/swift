
@available(tvOS 8.0, *)
class MTLComputePipelineReflection : NSObject {
  var arguments: [MTLArgument] { get }
}
@available(tvOS 9.0, *)
class MTLComputePipelineDescriptor : NSObject, NSCopying {
  var label: String?
  var computeFunction: MTLFunction?
  var threadGroupSizeIsMultipleOfThreadExecutionWidth: Bool
  func reset()
}
@available(tvOS 8.0, *)
protocol MTLComputePipelineState : NSObjectProtocol {
  var device: MTLDevice { get }
  var maxTotalThreadsPerThreadgroup: Int { get }
  var threadExecutionWidth: Int { get }
}

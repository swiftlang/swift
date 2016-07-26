
@available(tvOS 9.0, *)
class MPSImageMedian : MPSUnaryImageKernel {
  var kernelDiameter: Int { get }
  init(device device: MTLDevice, kernelDiameter kernelDiameter: Int)
  @discardableResult
  class func maxKernelDiameter() -> Int
  @discardableResult
  class func minKernelDiameter() -> Int
}


@available(iOS 9.0, *)
class MPSImageAreaMax : MPSUnaryImageKernel {
  var kernelHeight: Int { get }
  var kernelWidth: Int { get }
  init(device device: MTLDevice, kernelWidth kernelWidth: Int, kernelHeight kernelHeight: Int)
}
@available(iOS 9.0, *)
class MPSImageAreaMin : MPSImageAreaMax {
}
@available(iOS 9.0, *)
class MPSImageDilate : MPSUnaryImageKernel {
  var kernelHeight: Int { get }
  var kernelWidth: Int { get }
  init(device device: MTLDevice, kernelWidth kernelWidth: Int, kernelHeight kernelHeight: Int, values values: UnsafePointer<Float>)
}
@available(iOS 9.0, *)
class MPSImageErode : MPSImageDilate {
}

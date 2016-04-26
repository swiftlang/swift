
@available(tvOS 9.0, *)
class MPSImageThresholdBinary : MPSUnaryImageKernel {
  init(device device: MTLDevice, thresholdValue thresholdValue: Float, maximumValue maximumValue: Float, linearGrayColorTransform transform: UnsafePointer<Float>?)
  var thresholdValue: Float { get }
  var maximumValue: Float { get }
  var transform: UnsafePointer<Float> { get }
}
@available(tvOS 9.0, *)
class MPSImageThresholdBinaryInverse : MPSUnaryImageKernel {
  init(device device: MTLDevice, thresholdValue thresholdValue: Float, maximumValue maximumValue: Float, linearGrayColorTransform transform: UnsafePointer<Float>?)
  var thresholdValue: Float { get }
  var maximumValue: Float { get }
  var transform: UnsafePointer<Float> { get }
}
@available(tvOS 9.0, *)
class MPSImageThresholdTruncate : MPSUnaryImageKernel {
  init(device device: MTLDevice, thresholdValue thresholdValue: Float, linearGrayColorTransform transform: UnsafePointer<Float>?)
  var thresholdValue: Float { get }
  var transform: UnsafePointer<Float> { get }
}
@available(tvOS 9.0, *)
class MPSImageThresholdToZero : MPSUnaryImageKernel {
  init(device device: MTLDevice, thresholdValue thresholdValue: Float, linearGrayColorTransform transform: UnsafePointer<Float>?)
  var thresholdValue: Float { get }
  var transform: UnsafePointer<Float> { get }
}
@available(tvOS 9.0, *)
class MPSImageThresholdToZeroInverse : MPSUnaryImageKernel {
  init(device device: MTLDevice, thresholdValue thresholdValue: Float, linearGrayColorTransform transform: UnsafePointer<Float>?)
  var thresholdValue: Float { get }
  var transform: UnsafePointer<Float> { get }
}

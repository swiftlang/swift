
@available(iOS 9.0, *)
class MPSImageLanczosScale : MPSUnaryImageKernel {
  var scaleTransform: UnsafePointer<MPSScaleTransform>?
}

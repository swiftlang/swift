
struct MPSKernelOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  @available(iOS 9.0, *)
  static var none: MPSKernelOptions { get }
  @available(iOS 9.0, *)
  static var skipAPIValidation: MPSKernelOptions { get }
  @available(iOS 9.0, *)
  static var allowReducedPrecision: MPSKernelOptions { get }
}
enum MPSImageEdgeMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  @available(iOS 9.0, *)
  case zero
  @available(iOS 9.0, *)
  case clamp
}
struct MPSOffset {
  var x: Int
  var y: Int
  var z: Int
  init()
  init(x x: Int, y y: Int, z z: Int)
}
struct MPSOrigin {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
struct MPSSize {
  var width: Double
  var height: Double
  var depth: Double
  init()
  init(width width: Double, height height: Double, depth depth: Double)
}
struct MPSRegion {
  var origin: MPSOrigin
  var size: MPSSize
  init()
  init(origin origin: MPSOrigin, size size: MPSSize)
}
let MPSRectNoClip: MTLRegion
struct MPSScaleTransform {
  var scaleX: Double
  var scaleY: Double
  var translateX: Double
  var translateY: Double
  init()
  init(scaleX scaleX: Double, scaleY scaleY: Double, translateX translateX: Double, translateY translateY: Double)
}
typealias MPSCopyAllocator = (MPSKernel, MTLCommandBuffer, MTLTexture) -> MTLTexture

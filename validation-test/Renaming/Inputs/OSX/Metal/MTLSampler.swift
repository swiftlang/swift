
@available(OSX 10.11, *)
enum MTLSamplerMinMagFilter : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case nearest
  case linear
}
@available(OSX 10.11, *)
enum MTLSamplerMipFilter : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notMipmapped
  case nearest
  case linear
}
@available(OSX 10.11, *)
enum MTLSamplerAddressMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case clampToEdge
  @available(OSX 10.11, *)
  case mirrorClampToEdge
  case `repeat`
  case mirrorRepeat
  case clampToZero
}
@available(OSX 10.11, *)
class MTLSamplerDescriptor : NSObject, NSCopying {
  var minFilter: MTLSamplerMinMagFilter
  var magFilter: MTLSamplerMinMagFilter
  var mipFilter: MTLSamplerMipFilter
  var maxAnisotropy: Int
  var sAddressMode: MTLSamplerAddressMode
  var tAddressMode: MTLSamplerAddressMode
  var rAddressMode: MTLSamplerAddressMode
  var normalizedCoordinates: Bool
  var lodMinClamp: Float
  var lodMaxClamp: Float
  @available(OSX 10.11, *)
  var compareFunction: MTLCompareFunction
  var label: String?
}
@available(OSX 10.11, *)
protocol MTLSamplerState : NSObjectProtocol {
  var label: String? { get }
  var device: MTLDevice { get }
}

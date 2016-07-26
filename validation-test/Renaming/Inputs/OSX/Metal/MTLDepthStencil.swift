
@available(OSX 10.11, *)
enum MTLCompareFunction : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case never
  case less
  case equal
  case lessEqual
  case greater
  case notEqual
  case greaterEqual
  case always
}
@available(OSX 10.11, *)
enum MTLStencilOperation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case keep
  case zero
  case replace
  case incrementClamp
  case decrementClamp
  case invert
  case incrementWrap
  case decrementWrap
}
@available(OSX 10.11, *)
class MTLStencilDescriptor : NSObject, NSCopying {
  var stencilCompareFunction: MTLCompareFunction
  var stencilFailureOperation: MTLStencilOperation
  var depthFailureOperation: MTLStencilOperation
  var depthStencilPassOperation: MTLStencilOperation
  var readMask: UInt32
  var writeMask: UInt32
}
@available(OSX 10.11, *)
class MTLDepthStencilDescriptor : NSObject, NSCopying {
  var depthCompareFunction: MTLCompareFunction
  var isDepthWriteEnabled: Bool
  @NSCopying var frontFaceStencil: MTLStencilDescriptor!
  @NSCopying var backFaceStencil: MTLStencilDescriptor!
  var label: String?
}
@available(OSX 10.11, *)
protocol MTLDepthStencilState : NSObjectProtocol {
  var label: String? { get }
  var device: MTLDevice { get }
}

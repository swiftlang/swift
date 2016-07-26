
@available(OSX 10.11, *)
enum MTLLoadAction : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case dontCare
  case load
  case clear
}
@available(OSX 10.11, *)
enum MTLStoreAction : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case dontCare
  case store
  case multisampleResolve
}
struct MTLClearColor {
  var red: Double
  var green: Double
  var blue: Double
  var alpha: Double
  init()
  init(red red: Double, green green: Double, blue blue: Double, alpha alpha: Double)
}
@available(OSX 10.11, *)
class MTLRenderPassAttachmentDescriptor : NSObject, NSCopying {
  var texture: MTLTexture?
  var level: Int
  var slice: Int
  var depthPlane: Int
  var resolveTexture: MTLTexture?
  var resolveLevel: Int
  var resolveSlice: Int
  var resolveDepthPlane: Int
  var loadAction: MTLLoadAction
  var storeAction: MTLStoreAction
}
@available(OSX 10.11, *)
class MTLRenderPassColorAttachmentDescriptor : MTLRenderPassAttachmentDescriptor {
  var clearColor: MTLClearColor
}
@available(OSX 10.11, *)
class MTLRenderPassDepthAttachmentDescriptor : MTLRenderPassAttachmentDescriptor {
  var clearDepth: Double
}
@available(OSX 10.11, *)
class MTLRenderPassStencilAttachmentDescriptor : MTLRenderPassAttachmentDescriptor {
  var clearStencil: UInt32
}
@available(OSX 10.11, *)
class MTLRenderPassColorAttachmentDescriptorArray : NSObject {
  subscript(_ attachmentIndex: Int) -> MTLRenderPassColorAttachmentDescriptor!
}
@available(OSX 10.11, *)
class MTLRenderPassDescriptor : NSObject, NSCopying {
  var colorAttachments: MTLRenderPassColorAttachmentDescriptorArray { get }
  @NSCopying var depthAttachment: MTLRenderPassDepthAttachmentDescriptor!
  @NSCopying var stencilAttachment: MTLRenderPassStencilAttachmentDescriptor!
  var visibilityResultBuffer: MTLBuffer?
  @available(OSX 10.11, *)
  var renderTargetArrayLength: Int
}
@discardableResult
func MTLClearColorMake(_ red: Double, _ green: Double, _ blue: Double, _ alpha: Double) -> MTLClearColor

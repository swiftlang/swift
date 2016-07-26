
@available(OSX 10.11, *)
enum MTLVertexFormat : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case invalid
  case uChar2
  case uChar3
  case uChar4
  case char2
  case char3
  case char4
  case uChar2Normalized
  case uChar3Normalized
  case uChar4Normalized
  case char2Normalized
  case char3Normalized
  case char4Normalized
  case uShort2
  case uShort3
  case uShort4
  case short2
  case short3
  case short4
  case uShort2Normalized
  case uShort3Normalized
  case uShort4Normalized
  case short2Normalized
  case short3Normalized
  case short4Normalized
  case half2
  case half3
  case half4
  case float
  case float2
  case float3
  case float4
  case int
  case int2
  case int3
  case int4
  case uInt
  case uInt2
  case uInt3
  case uInt4
  case int1010102Normalized
  case uInt1010102Normalized
}
@available(OSX 10.11, *)
enum MTLVertexStepFunction : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case constant
  case perVertex
  case perInstance
}
@available(OSX 10.11, *)
class MTLVertexBufferLayoutDescriptor : NSObject, NSCopying {
  var stride: Int
  var stepFunction: MTLVertexStepFunction
  var stepRate: Int
}
@available(OSX 10.11, *)
class MTLVertexBufferLayoutDescriptorArray : NSObject {
  subscript(_ index: Int) -> MTLVertexBufferLayoutDescriptor!
}
@available(OSX 10.11, *)
class MTLVertexAttributeDescriptor : NSObject, NSCopying {
  var format: MTLVertexFormat
  var offset: Int
  var bufferIndex: Int
}
@available(OSX 10.11, *)
class MTLVertexAttributeDescriptorArray : NSObject {
  subscript(_ index: Int) -> MTLVertexAttributeDescriptor!
}
@available(OSX 10.11, *)
class MTLVertexDescriptor : NSObject, NSCopying {
  var layouts: MTLVertexBufferLayoutDescriptorArray { get }
  var attributes: MTLVertexAttributeDescriptorArray { get }
  func reset()
}

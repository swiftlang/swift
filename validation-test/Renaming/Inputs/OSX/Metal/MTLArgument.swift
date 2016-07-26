
@available(OSX 10.11, *)
enum MTLDataType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case `struct`
  case array
  case float
  case float2
  case float3
  case float4
  case float2x2
  case float2x3
  case float2x4
  case float3x2
  case float3x3
  case float3x4
  case float4x2
  case float4x3
  case float4x4
  case half
  case half2
  case half3
  case half4
  case half2x2
  case half2x3
  case half2x4
  case half3x2
  case half3x3
  case half3x4
  case half4x2
  case half4x3
  case half4x4
  case int
  case int2
  case int3
  case int4
  case uInt
  case uInt2
  case uInt3
  case uInt4
  case short
  case short2
  case short3
  case short4
  case uShort
  case uShort2
  case uShort3
  case uShort4
  case char
  case char2
  case char3
  case char4
  case uChar
  case uChar2
  case uChar3
  case uChar4
  case bool
  case bool2
  case bool3
  case bool4
}
@available(OSX 10.11, *)
enum MTLArgumentType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case buffer
  case threadgroupMemory
  case texture
  case sampler
}
@available(OSX 10.11, *)
enum MTLArgumentAccess : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case readOnly
  case readWrite
  case writeOnly
}
@available(OSX 10.11, *)
class MTLStructMember : NSObject {
  var name: String { get }
  var offset: Int { get }
  var dataType: MTLDataType { get }
  @discardableResult
  func structType() -> MTLStructType?
  @discardableResult
  func arrayType() -> MTLArrayType?
}
@available(OSX 10.11, *)
class MTLStructType : NSObject {
  var members: [MTLStructMember] { get }
  @discardableResult
  func member(byName name: String) -> MTLStructMember?
}
@available(OSX 10.11, *)
class MTLArrayType : NSObject {
  var arrayLength: Int { get }
  var elementType: MTLDataType { get }
  var stride: Int { get }
  @discardableResult
  func elementStructType() -> MTLStructType?
  @discardableResult
  func element() -> MTLArrayType?
}
@available(OSX 10.11, *)
class MTLArgument : NSObject {
  var name: String { get }
  var type: MTLArgumentType { get }
  var access: MTLArgumentAccess { get }
  var index: Int { get }
  var isActive: Bool { get }
  var bufferAlignment: Int { get }
  var bufferDataSize: Int { get }
  var bufferDataType: MTLDataType { get }
  var bufferStructType: MTLStructType { get }
  var threadgroupMemoryAlignment: Int { get }
  var threadgroupMemoryDataSize: Int { get }
  var textureType: MTLTextureType { get }
  var textureDataType: MTLDataType { get }
}

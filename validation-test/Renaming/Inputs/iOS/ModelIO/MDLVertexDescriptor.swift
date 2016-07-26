
let MDLVertexAttributeAnisotropy: String
let MDLVertexAttributeBinormal: String
let MDLVertexAttributeBitangent: String
let MDLVertexAttributeColor: String
let MDLVertexAttributeEdgeCrease: String
let MDLVertexAttributeJointIndices: String
let MDLVertexAttributeJointWeights: String
let MDLVertexAttributeNormal: String
let MDLVertexAttributeOcclusionValue: String
let MDLVertexAttributePosition: String
let MDLVertexAttributeShadingBasisU: String
let MDLVertexAttributeShadingBasisV: String
let MDLVertexAttributeSubdivisionStencil: String
let MDLVertexAttributeTangent: String
let MDLVertexAttributeTextureCoordinate: String
enum MDLVertexFormat : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case invalid
  case packedBit
  case uCharBits
  case charBits
  case uCharNormalizedBits
  case charNormalizedBits
  case uShortBits
  case shortBits
  case uShortNormalizedBits
  case shortNormalizedBits
  case uIntBits
  case intBits
  case halfBits
  case floatBits
  case uChar
  case uChar2
  case uChar3
  case uChar4
  case char
  case char2
  case char3
  case char4
  case uCharNormalized
  case uChar2Normalized
  case uChar3Normalized
  case uChar4Normalized
  case charNormalized
  case char2Normalized
  case char3Normalized
  case char4Normalized
  case uShort
  case uShort2
  case uShort3
  case uShort4
  case short
  case short2
  case short3
  case short4
  case uShortNormalized
  case uShort2Normalized
  case uShort3Normalized
  case uShort4Normalized
  case shortNormalized
  case short2Normalized
  case short3Normalized
  case short4Normalized
  case uInt
  case uInt2
  case uInt3
  case uInt4
  case int
  case int2
  case int3
  case int4
  case half
  case half2
  case half3
  case half4
  case float
  case float2
  case float3
  case float4
  case int1010102Normalized
  case uInt1010102Normalized
}
@available(iOS 9.0, *)
class MDLVertexBufferLayout : NSObject, NSCopying {
  var stride: Int
}
@available(iOS 9.0, *)
class MDLVertexAttribute : NSObject, NSCopying {
  init(name name: String, format format: MDLVertexFormat, offset offset: Int, bufferIndex bufferIndex: Int)
  var name: String
  var format: MDLVertexFormat
  var offset: Int
  var bufferIndex: Int
  var initializationValue: vector_float4
}
@available(iOS 9.0, *)
class MDLVertexDescriptor : NSObject, NSCopying {
  init(vertexDescriptor vertexDescriptor: MDLVertexDescriptor)
  @discardableResult
  func attributeNamed(_ name: String) -> MDLVertexAttribute?
  func addOrReplaceAttribute(_ attribute: MDLVertexAttribute)
  var attributes: NSMutableArray
  var layouts: NSMutableArray
  func reset()
  func setPackedStrides()
  func setPackedOffsets()
}

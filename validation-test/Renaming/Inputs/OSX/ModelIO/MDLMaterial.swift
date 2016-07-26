
enum MDLMaterialSemantic : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case baseColor
  case subsurface
  case metallic
  case specular
  case specularExponent
  case specularTint
  case roughness
  case anisotropic
  case anisotropicRotation
  case sheen
  case sheenTint
  case clearcoat
  case clearcoatGloss
  case emission
  case bump
  case opacity
  case interfaceIndexOfRefraction
  case materialIndexOfRefraction
  case objectSpaceNormal
  case tangentSpaceNormal
  case displacement
  case displacementScale
  case ambientOcclusion
  case ambientOcclusionScale
  case none
  case userDefined
}
enum MDLMaterialPropertyType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case string
  case URL
  case texture
  case color
  case float
  case float2
  case float3
  case float4
  case matrix44
}
enum MDLMaterialTextureWrapMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case clamp
  case `repeat`
  case mirror
}
enum MDLMaterialTextureFilterMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case nearest
  case linear
}
enum MDLMaterialMipMapFilterMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case nearest
  case linear
}
@available(OSX 10.11, *)
class MDLTextureFilter : NSObject {
  var sWrapMode: MDLMaterialTextureWrapMode
  var tWrapMode: MDLMaterialTextureWrapMode
  var rWrapMode: MDLMaterialTextureWrapMode
  var minFilter: MDLMaterialTextureFilterMode
  var magFilter: MDLMaterialTextureFilterMode
  var mipFilter: MDLMaterialMipMapFilterMode
}
@available(OSX 10.11, *)
class MDLTextureSampler : NSObject {
  var texture: MDLTexture?
  var hardwareFilter: MDLTextureFilter?
  var transform: MDLTransform?
}
@available(OSX 10.11, *)
class MDLMaterialProperty : NSObject, MDLNamed {
  init(name name: String, semantic semantic: MDLMaterialSemantic)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, float value: Float)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, float2 value: vector_float2)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, float3 value: vector_float3)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, float4 value: vector_float4)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, matrix4x4 value: matrix_float4x4)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, url URL: NSURL?)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, string string: String?)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, textureSampler textureSampler: MDLTextureSampler?)
  convenience init(name name: String, semantic semantic: MDLMaterialSemantic, color color: CGColor)
  func setProperties(_ property: MDLMaterialProperty)
  var semantic: MDLMaterialSemantic
  var type: MDLMaterialPropertyType { get }
  var stringValue: String?
  @NSCopying var urlValue: NSURL?
  var textureSamplerValue: MDLTextureSampler?
  var color: CGColor?
  var floatValue: Float
  var float2Value: vector_float2
  var float3Value: vector_float3
  var float4Value: vector_float4
  var matrix4x4: matrix_float4x4
}
@available(OSX 10.11, *)
class MDLScatteringFunction : NSObject, MDLNamed {
  var baseColor: MDLMaterialProperty { get }
  var emission: MDLMaterialProperty { get }
  var specular: MDLMaterialProperty { get }
  var materialIndexOfRefraction: MDLMaterialProperty { get }
  var interfaceIndexOfRefraction: MDLMaterialProperty { get }
  var normal: MDLMaterialProperty { get }
  var ambientOcclusion: MDLMaterialProperty { get }
  var ambientOcclusionScale: MDLMaterialProperty { get }
}
@available(OSX 10.11, *)
class MDLPhysicallyPlausibleScatteringFunction : MDLScatteringFunction {
  var version: Int { get }
  var subsurface: MDLMaterialProperty { get }
  var metallic: MDLMaterialProperty { get }
  var specularAmount: MDLMaterialProperty { get }
  var specularTint: MDLMaterialProperty { get }
  var roughness: MDLMaterialProperty { get }
  var anisotropic: MDLMaterialProperty { get }
  var anisotropicRotation: MDLMaterialProperty { get }
  var sheen: MDLMaterialProperty { get }
  var sheenTint: MDLMaterialProperty { get }
  var clearcoat: MDLMaterialProperty { get }
  var clearcoatGloss: MDLMaterialProperty { get }
}
@available(OSX 10.11, *)
class MDLMaterial : NSObject, MDLNamed, NSFastEnumeration {
  init(name name: String, scatteringFunction scatteringFunction: MDLScatteringFunction)
  func setProperty(_ property: MDLMaterialProperty)
  func remove(_ property: MDLMaterialProperty)
  @discardableResult
  func propertyNamed(_ name: String) -> MDLMaterialProperty?
  @discardableResult
  func property(with semantic: MDLMaterialSemantic) -> MDLMaterialProperty?
  func removeAllProperties()
  var scatteringFunction: MDLScatteringFunction { get }
  var base: MDLMaterial?
  subscript(_ idx: Int) -> MDLMaterialProperty? { get }
  subscript(_ name: String) -> MDLMaterialProperty? { get }
  var count: Int { get }
}

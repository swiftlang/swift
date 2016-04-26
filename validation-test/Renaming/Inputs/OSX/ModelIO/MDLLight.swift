
enum MDLLightType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case ambient
  case directional
  case spot
  case point
  case linear
  case discArea
  case rectangularArea
  case superElliptical
  case photometric
  case probe
  case environment
}
@available(OSX 10.11, *)
class MDLLight : MDLObject {
  @discardableResult
  func irradiance(atPoint point: vector_float3) -> Unmanaged<CGColor>
  @discardableResult
  func irradiance(atPoint point: vector_float3, colorSpace colorSpace: CGColorSpace) -> Unmanaged<CGColor>
  var lightType: MDLLightType
}
@available(OSX 10.11, *)
class MDLPhysicallyPlausibleLight : MDLLight {
  func setColorByTemperature(_ temperature: Float)
  var color: CGColor?
  var lumens: Float
  var innerConeAngle: Float
  var outerConeAngle: Float
  var attenuationStartDistance: Float
  var attenuationEndDistance: Float
}
@available(OSX 10.11, *)
class MDLAreaLight : MDLPhysicallyPlausibleLight {
  var areaRadius: Float
  var superEllipticPower: vector_float2
  var aspect: Float
}
@available(OSX 10.11, *)
class MDLPhotometricLight : MDLPhysicallyPlausibleLight {
  init?(iesProfile URL: NSURL)
  func generateSphericalHarmonics(fromLight sphericalHarmonicsLevel: Int)
  func generateCubemap(fromLight textureSize: Int)
  var lightCubeMap: MDLTexture? { get }
  var sphericalHarmonicsLevel: Int { get }
  @NSCopying var sphericalHarmonicsCoefficients: NSData? { get }
}
@available(OSX 10.11, *)
class MDLLightProbe : MDLLight {
  init(reflectiveTexture reflectiveTexture: MDLTexture?, irradianceTexture irradianceTexture: MDLTexture?)
  func generateSphericalHarmonics(fromIrradiance sphericalHarmonicsLevel: Int)
  var reflectiveTexture: MDLTexture? { get }
  var irradianceTexture: MDLTexture? { get }
  var sphericalHarmonicsLevel: Int { get }
  @NSCopying var sphericalHarmonicsCoefficients: NSData? { get }
}
extension MDLLightProbe {
  /*not inherited*/ init?(textureSize textureSize: Int, forLocation transform: MDLTransform, lightsToConsider lightsToConsider: [MDLLight], objectsToConsider objectsToConsider: [MDLObject], reflectiveCubemap reflectiveCubemap: MDLTexture?, irradianceCubemap irradianceCubemap: MDLTexture?)
}

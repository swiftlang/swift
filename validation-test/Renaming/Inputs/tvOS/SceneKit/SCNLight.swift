
let SCNLightTypeAmbient: String
let SCNLightTypeOmni: String
let SCNLightTypeDirectional: String
let SCNLightTypeSpot: String
@available(tvOS 8.0, *)
enum SCNShadowMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case forward
  case deferred
  case modulated
}
@available(tvOS 8.0, *)
class SCNLight : NSObject, SCNAnimatable, SCNTechniqueSupport, NSCopying, NSSecureCoding {
  var type: String
  var color: AnyObject
  var name: String?
  var castsShadow: Bool
  var shadowColor: AnyObject
  var shadowRadius: CGFloat
  @available(tvOS 8.0, *)
  var shadowMapSize: CGSize
  @available(tvOS 8.0, *)
  var shadowSampleCount: Int
  @available(tvOS 8.0, *)
  var shadowMode: SCNShadowMode
  @available(tvOS 8.0, *)
  var shadowBias: CGFloat
  @available(tvOS 8.0, *)
  var orthographicScale: CGFloat
  @available(tvOS 8.0, *)
  var zNear: CGFloat
  @available(tvOS 8.0, *)
  var zFar: CGFloat
  @available(tvOS 8.0, *)
  var attenuationStartDistance: CGFloat
  @available(tvOS 8.0, *)
  var attenuationEndDistance: CGFloat
  @available(tvOS 8.0, *)
  var attenuationFalloffExponent: CGFloat
  @available(tvOS 8.0, *)
  var spotInnerAngle: CGFloat
  @available(tvOS 8.0, *)
  var spotOuterAngle: CGFloat
  @available(tvOS 8.0, *)
  var gobo: SCNMaterialProperty? { get }
  @available(tvOS 8.0, *)
  var categoryBitMask: Int
}

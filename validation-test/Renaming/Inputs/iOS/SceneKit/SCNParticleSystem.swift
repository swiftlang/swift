
@available(iOS 8.0, *)
let SCNParticlePropertyPosition: String
@available(iOS 8.0, *)
let SCNParticlePropertyAngle: String
@available(iOS 8.0, *)
let SCNParticlePropertyRotationAxis: String
@available(iOS 8.0, *)
let SCNParticlePropertyVelocity: String
@available(iOS 8.0, *)
let SCNParticlePropertyAngularVelocity: String
@available(iOS 8.0, *)
let SCNParticlePropertyLife: String
@available(iOS 8.0, *)
let SCNParticlePropertyColor: String
@available(iOS 8.0, *)
let SCNParticlePropertyOpacity: String
@available(iOS 8.0, *)
let SCNParticlePropertySize: String
@available(iOS 8.0, *)
let SCNParticlePropertyFrame: String
@available(iOS 8.0, *)
let SCNParticlePropertyFrameRate: String
@available(iOS 8.0, *)
let SCNParticlePropertyBounce: String
@available(iOS 8.0, *)
let SCNParticlePropertyCharge: String
@available(iOS 8.0, *)
let SCNParticlePropertyFriction: String
@available(iOS 8.0, *)
let SCNParticlePropertyContactPoint: String
@available(iOS 8.0, *)
let SCNParticlePropertyContactNormal: String
typealias SCNParticleEventBlock = (UnsafeMutablePointer<UnsafeMutablePointer<Void>>, UnsafeMutablePointer<Int>, UnsafeMutablePointer<UInt32>?, Int) -> Void
typealias SCNParticleModifierBlock = (UnsafeMutablePointer<UnsafeMutablePointer<Void>>, UnsafeMutablePointer<Int>, Int, Int, Float) -> Void
@available(iOS 8.0, *)
enum SCNParticleSortingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case projectedDepth
  case distance
  case oldestFirst
  case youngestFirst
}
@available(iOS 8.0, *)
enum SCNParticleBlendMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case additive
  case subtract
  case multiply
  case screen
  case alpha
  case replace
}
@available(iOS 8.0, *)
enum SCNParticleOrientationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case billboardScreenAligned
  case billboardViewAligned
  case free
  case billboardYAligned
}
@available(iOS 8.0, *)
enum SCNParticleBirthLocation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case surface
  case volume
  case vertex
}
@available(iOS 8.0, *)
enum SCNParticleBirthDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case constant
  case surfaceNormal
  case random
}
@available(iOS 8.0, *)
enum SCNParticleImageSequenceAnimationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `repeat`
  case clamp
  case autoReverse
}
@available(iOS 8.0, *)
enum SCNParticleInputMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case overLife
  case overDistance
  case overOtherProperty
}
@available(iOS 8.0, *)
enum SCNParticleModifierStage : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case preDynamics
  case postDynamics
  case preCollision
  case postCollision
}
@available(iOS 8.0, *)
enum SCNParticleEvent : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case birth
  case death
  case collision
}
@available(iOS 8.0, *)
class SCNParticlePropertyController : NSObject, NSSecureCoding, NSCopying {
  convenience init(animation animation: CAAnimation)
  var animation: CAAnimation
  var inputMode: SCNParticleInputMode
  var inputScale: CGFloat
  var inputBias: CGFloat
  weak var inputOrigin: @sil_weak SCNNode?
  var inputProperty: String?
}
@available(iOS 8.0, *)
class SCNParticleSystem : NSObject, NSCopying, NSSecureCoding, SCNAnimatable {
  convenience init?(named name: String, inDirectory directory: String?)
  var emissionDuration: CGFloat
  var emissionDurationVariation: CGFloat
  var idleDuration: CGFloat
  var idleDurationVariation: CGFloat
  var loops: Bool
  var birthRate: CGFloat
  var birthRateVariation: CGFloat
  var warmupDuration: CGFloat
  var emitterShape: SCNGeometry?
  var birthLocation: SCNParticleBirthLocation
  var birthDirection: SCNParticleBirthDirection
  var spreadingAngle: CGFloat
  var emittingDirection: SCNVector3
  var acceleration: SCNVector3
  var isLocal: Bool
  var particleAngle: CGFloat
  var particleAngleVariation: CGFloat
  var particleVelocity: CGFloat
  var particleVelocityVariation: CGFloat
  var particleAngularVelocity: CGFloat
  var particleAngularVelocityVariation: CGFloat
  var particleLifeSpan: CGFloat
  var particleLifeSpanVariation: CGFloat
  var systemSpawnedOnDying: SCNParticleSystem?
  var systemSpawnedOnCollision: SCNParticleSystem?
  var systemSpawnedOnLiving: SCNParticleSystem?
  var particleImage: AnyObject?
  var imageSequenceColumnCount: Int
  var imageSequenceRowCount: Int
  var imageSequenceInitialFrame: CGFloat
  var imageSequenceInitialFrameVariation: CGFloat
  var imageSequenceFrameRate: CGFloat
  var imageSequenceFrameRateVariation: CGFloat
  var imageSequenceAnimationMode: SCNParticleImageSequenceAnimationMode
  var particleColor: UIColor
  var particleColorVariation: SCNVector4
  var particleSize: CGFloat
  var particleSizeVariation: CGFloat
  var blendMode: SCNParticleBlendMode
  var isBlackPassEnabled: Bool
  var orientationMode: SCNParticleOrientationMode
  var sortingMode: SCNParticleSortingMode
  var isLightingEnabled: Bool
  var affectedByGravity: Bool
  var affectedByPhysicsFields: Bool
  var particleDiesOnCollision: Bool
  var colliderNodes: [SCNNode]?
  var particleMass: CGFloat
  var particleMassVariation: CGFloat
  var particleBounce: CGFloat
  var particleBounceVariation: CGFloat
  var particleFriction: CGFloat
  var particleFrictionVariation: CGFloat
  var particleCharge: CGFloat
  var particleChargeVariation: CGFloat
  var dampingFactor: CGFloat
  var speedFactor: CGFloat
  var stretchFactor: CGFloat
  var fresnelExponent: CGFloat
  var propertyControllers: [String : SCNParticlePropertyController]?
  func reset()
  func handle(_ event: SCNParticleEvent, forProperties properties: [String], with block: SCNParticleEventBlock)
  func addModifier(forProperties properties: [String], at stage: SCNParticleModifierStage, with block: SCNParticleModifierBlock)
  func removeModifiers(of stage: SCNParticleModifierStage)
  func removeAllModifiers()
}
extension SCNNode {
  @available(iOS 8.0, *)
  func addParticleSystem(_ system: SCNParticleSystem)
  @available(iOS 8.0, *)
  func removeAllParticleSystems()
  @available(iOS 8.0, *)
  func removeParticleSystem(_ system: SCNParticleSystem)
  @available(iOS 8.0, *)
  var particleSystems: [SCNParticleSystem]? { get }
}
extension SCNScene {
  @available(iOS 8.0, *)
  func addParticleSystem(_ system: SCNParticleSystem, withTransform transform: SCNMatrix4)
  @available(iOS 8.0, *)
  func removeAllParticleSystems()
  @available(iOS 8.0, *)
  func removeParticleSystem(_ system: SCNParticleSystem)
  @available(iOS 8.0, *)
  var particleSystems: [SCNParticleSystem]? { get }
}

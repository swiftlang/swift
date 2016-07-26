
@available(OSX 10.11, *)
enum SKParticleRenderOrder : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case oldestLast
  case oldestFirst
  case dontCare
}
class SKEmitterNode : SKNode {
  func advanceSimulationTime(_ sec: NSTimeInterval)
  func resetSimulation()
  var particleTexture: SKTexture?
  var particleBlendMode: SKBlendMode
  var particleColor: NSColor
  var particleColorRedRange: CGFloat
  var particleColorGreenRange: CGFloat
  var particleColorBlueRange: CGFloat
  var particleColorAlphaRange: CGFloat
  var particleColorRedSpeed: CGFloat
  var particleColorGreenSpeed: CGFloat
  var particleColorBlueSpeed: CGFloat
  var particleColorAlphaSpeed: CGFloat
  var particleColorSequence: SKKeyframeSequence?
  var particleColorBlendFactor: CGFloat
  var particleColorBlendFactorRange: CGFloat
  var particleColorBlendFactorSpeed: CGFloat
  var particleColorBlendFactorSequence: SKKeyframeSequence?
  var particlePosition: CGPoint
  var particlePositionRange: CGVector
  var particleSpeed: CGFloat
  var particleSpeedRange: CGFloat
  var emissionAngle: CGFloat
  var emissionAngleRange: CGFloat
  var xAcceleration: CGFloat
  var yAcceleration: CGFloat
  var particleBirthRate: CGFloat
  var numParticlesToEmit: Int
  var particleLifetime: CGFloat
  var particleLifetimeRange: CGFloat
  var particleRotation: CGFloat
  var particleRotationRange: CGFloat
  var particleRotationSpeed: CGFloat
  var particleSize: CGSize
  var particleScale: CGFloat
  var particleScaleRange: CGFloat
  var particleScaleSpeed: CGFloat
  var particleScaleSequence: SKKeyframeSequence?
  var particleAlpha: CGFloat
  var particleAlphaRange: CGFloat
  var particleAlphaSpeed: CGFloat
  var particleAlphaSequence: SKKeyframeSequence?
  @NSCopying var particleAction: SKAction?
  var fieldBitMask: UInt32
  weak var targetNode: @sil_weak SKNode?
  var shader: SKShader?
  var particleZPosition: CGFloat
  @available(OSX 10.11, *)
  var particleRenderOrder: SKParticleRenderOrder
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var particleZPositionRange: CGFloat
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  var particleZPositionSpeed: CGFloat
}

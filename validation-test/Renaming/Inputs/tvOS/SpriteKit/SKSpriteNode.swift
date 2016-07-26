
class SKSpriteNode : SKNode {
  convenience init(texture texture: SKTexture?, size size: CGSize)
  convenience init(texture texture: SKTexture?, normalMap normalMap: SKTexture?)
  convenience init(imageNamed name: String, normalMapped generateNormalMap: Bool)
  init(texture texture: SKTexture?, color color: UIColor, size size: CGSize)
  convenience init(texture texture: SKTexture?)
  convenience init(imageNamed name: String)
  convenience init(color color: UIColor, size size: CGSize)
  var texture: SKTexture?
  @available(tvOS 8.0, *)
  var normalTexture: SKTexture?
  @available(tvOS 8.0, *)
  var lightingBitMask: UInt32
  @available(tvOS 8.0, *)
  var shadowCastBitMask: UInt32
  @available(tvOS 8.0, *)
  var shadowedBitMask: UInt32
  var centerRect: CGRect
  var colorBlendFactor: CGFloat
  var color: UIColor
  var blendMode: SKBlendMode
  var anchorPoint: CGPoint
  var size: CGSize
  @available(tvOS 8.0, *)
  var shader: SKShader?
}

extension SKSpriteNode : CustomPlaygroundQuickLookable {
}


class SKSpriteNode : SKNode {
  convenience init(texture texture: SKTexture?, size size: CGSize)
  convenience init(texture texture: SKTexture?, normalMap normalMap: SKTexture?)
  convenience init(imageNamed name: String, normalMapped generateNormalMap: Bool)
  init(texture texture: SKTexture?, color color: NSColor, size size: CGSize)
  convenience init(texture texture: SKTexture?)
  convenience init(imageNamed name: String)
  convenience init(color color: NSColor, size size: CGSize)
  var texture: SKTexture?
  @available(OSX 10.10, *)
  var normalTexture: SKTexture?
  @available(OSX 10.10, *)
  var lightingBitMask: UInt32
  @available(OSX 10.10, *)
  var shadowCastBitMask: UInt32
  @available(OSX 10.10, *)
  var shadowedBitMask: UInt32
  var centerRect: CGRect
  var colorBlendFactor: CGFloat
  var color: NSColor
  var blendMode: SKBlendMode
  var anchorPoint: CGPoint
  var size: CGSize
  @available(OSX 10.10, *)
  var shader: SKShader?
}

extension SKSpriteNode : CustomPlaygroundQuickLookable {
}


@available(OSX 10.10, *)
class SKFieldNode : SKNode {
  var region: SKRegion?
  var strength: Float
  var falloff: Float
  var minimumRadius: Float
  var isEnabled: Bool
  var isExclusive: Bool
  var categoryBitMask: UInt32
  var direction: vector_float3
  var smoothness: Float
  var animationSpeed: Float
  var texture: SKTexture?
  @discardableResult
  class func dragField() -> SKFieldNode
  @discardableResult
  class func vortexField() -> SKFieldNode
  @discardableResult
  class func radialGravityField() -> SKFieldNode
  @discardableResult
  class func linearGravityField(withVector direction: vector_float3) -> SKFieldNode
  @discardableResult
  class func velocityField(withVector direction: vector_float3) -> SKFieldNode
  @discardableResult
  class func velocityField(with velocityTexture: SKTexture) -> SKFieldNode
  @discardableResult
  class func noiseField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> SKFieldNode
  @discardableResult
  class func turbulenceField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> SKFieldNode
  @discardableResult
  class func springField() -> SKFieldNode
  @discardableResult
  class func electricField() -> SKFieldNode
  @discardableResult
  class func magneticField() -> SKFieldNode
  @discardableResult
  class func customField(evaluationBlock block: SKFieldForceEvaluator) -> SKFieldNode
}
typealias SKFieldForceEvaluator = (vector_float3, vector_float3, Float, Float, NSTimeInterval) -> vector_float3

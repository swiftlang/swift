
@available(iOS 9.0, *)
class UIFieldBehavior : UIDynamicBehavior {
  func addItem(_ item: UIDynamicItem)
  func removeItem(_ item: UIDynamicItem)
  var items: [UIDynamicItem] { get }
  var position: CGPoint
  var region: UIRegion
  var strength: CGFloat
  var falloff: CGFloat
  var minimumRadius: CGFloat
  var direction: CGVector
  var smoothness: CGFloat
  var animationSpeed: CGFloat
  @discardableResult
  class func dragField() -> Self
  @discardableResult
  class func vortexField() -> Self
  @discardableResult
  class func radialGravityField(withPosition position: CGPoint) -> Self
  @discardableResult
  class func linearGravityField(with direction: CGVector) -> Self
  @discardableResult
  class func velocityField(with direction: CGVector) -> Self
  @discardableResult
  class func noiseField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> Self
  @discardableResult
  class func turbulenceField(withSmoothness smoothness: CGFloat, animationSpeed speed: CGFloat) -> Self
  @discardableResult
  class func springField() -> Self
  @discardableResult
  class func electricField() -> Self
  @discardableResult
  class func magneticField() -> Self
  @discardableResult
  class func field(evaluationBlock block: (UIFieldBehavior, CGPoint, CGVector, CGFloat, CGFloat, NSTimeInterval) -> CGVector) -> Self
}

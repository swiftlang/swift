
class SKShapeNode : SKNode {
  @available(tvOS 8.0, *)
  convenience init(path path: CGPath)
  @available(tvOS 8.0, *)
  convenience init(path path: CGPath, centered centered: Bool)
  @available(tvOS 8.0, *)
  convenience init(rect rect: CGRect)
  @available(tvOS 8.0, *)
  convenience init(rectOf size: CGSize)
  @available(tvOS 8.0, *)
  convenience init(rect rect: CGRect, cornerRadius cornerRadius: CGFloat)
  @available(tvOS 8.0, *)
  convenience init(rectOf size: CGSize, cornerRadius cornerRadius: CGFloat)
  @available(tvOS 8.0, *)
  convenience init(circleOfRadius radius: CGFloat)
  @available(tvOS 8.0, *)
  convenience init(ellipseIn rect: CGRect)
  @available(tvOS 8.0, *)
  convenience init(ellipseOf size: CGSize)
  @available(tvOS 8.0, *)
  convenience init(points points: UnsafeMutablePointer<CGPoint>, count numPoints: Int)
  @available(tvOS 8.0, *)
  convenience init(splinePoints points: UnsafeMutablePointer<CGPoint>, count numPoints: Int)
  var path: CGPath?
  var strokeColor: UIColor
  var fillColor: UIColor
  var blendMode: SKBlendMode
  var isAntialiased: Bool
  var lineWidth: CGFloat
  var glowWidth: CGFloat
  var lineCap: CGLineCap
  var lineJoin: CGLineJoin
  var miterLimit: CGFloat
  var lineLength: CGFloat { get }
  @available(tvOS 8.0, *)
  var fillTexture: SKTexture?
  @available(tvOS 8.0, *)
  var fillShader: SKShader?
  @available(tvOS 8.0, *)
  var strokeTexture: SKTexture?
  @available(tvOS 8.0, *)
  var strokeShader: SKShader?
}

extension SKShapeNode : CustomPlaygroundQuickLookable {
}

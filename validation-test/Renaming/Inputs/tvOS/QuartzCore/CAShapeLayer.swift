
class CAShapeLayer : CALayer {
  var path: CGPath?
  var fillColor: CGColor?
  var fillRule: String
  var strokeColor: CGColor?
  var strokeStart: CGFloat
  var strokeEnd: CGFloat
  var lineWidth: CGFloat
  var miterLimit: CGFloat
  var lineCap: String
  var lineJoin: String
  var lineDashPhase: CGFloat
  var lineDashPattern: [NSNumber]?
}
@available(tvOS 3.0, *)
let kCAFillRuleNonZero: String
@available(tvOS 3.0, *)
let kCAFillRuleEvenOdd: String
@available(tvOS 3.0, *)
let kCALineJoinMiter: String
@available(tvOS 3.0, *)
let kCALineJoinRound: String
@available(tvOS 3.0, *)
let kCALineJoinBevel: String
@available(tvOS 3.0, *)
let kCALineCapButt: String
@available(tvOS 3.0, *)
let kCALineCapRound: String
@available(tvOS 3.0, *)
let kCALineCapSquare: String

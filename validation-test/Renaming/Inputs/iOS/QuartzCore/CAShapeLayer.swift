
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
@available(iOS 3.0, *)
let kCAFillRuleNonZero: String
@available(iOS 3.0, *)
let kCAFillRuleEvenOdd: String
@available(iOS 3.0, *)
let kCALineJoinMiter: String
@available(iOS 3.0, *)
let kCALineJoinRound: String
@available(iOS 3.0, *)
let kCALineJoinBevel: String
@available(iOS 3.0, *)
let kCALineCapButt: String
@available(iOS 3.0, *)
let kCALineCapRound: String
@available(iOS 3.0, *)
let kCALineCapSquare: String

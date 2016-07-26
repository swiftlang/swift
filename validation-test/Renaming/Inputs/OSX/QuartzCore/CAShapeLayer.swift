
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
@available(OSX 10.6, *)
let kCAFillRuleNonZero: String
@available(OSX 10.6, *)
let kCAFillRuleEvenOdd: String
@available(OSX 10.6, *)
let kCALineJoinMiter: String
@available(OSX 10.6, *)
let kCALineJoinRound: String
@available(OSX 10.6, *)
let kCALineJoinBevel: String
@available(OSX 10.6, *)
let kCALineCapButt: String
@available(OSX 10.6, *)
let kCALineCapRound: String
@available(OSX 10.6, *)
let kCALineCapSquare: String

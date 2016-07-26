
class CAEmitterCell : NSObject, NSCoding, CAMediaTiming {
  @discardableResult
  class func defaultValue(forKey key: String) -> AnyObject?
  @discardableResult
  func shouldArchiveValue(forKey key: String) -> Bool
  var name: String?
  var isEnabled: Bool
  var birthRate: Float
  var lifetime: Float
  var lifetimeRange: Float
  var emissionLatitude: CGFloat
  var emissionLongitude: CGFloat
  var emissionRange: CGFloat
  var velocity: CGFloat
  var velocityRange: CGFloat
  var xAcceleration: CGFloat
  var yAcceleration: CGFloat
  var zAcceleration: CGFloat
  var scale: CGFloat
  var scaleRange: CGFloat
  var scaleSpeed: CGFloat
  var spin: CGFloat
  var spinRange: CGFloat
  var color: CGColor?
  var redRange: Float
  var greenRange: Float
  var blueRange: Float
  var alphaRange: Float
  var redSpeed: Float
  var greenSpeed: Float
  var blueSpeed: Float
  var alphaSpeed: Float
  var contents: AnyObject?
  var contentsRect: CGRect
  var contentsScale: CGFloat
  var minificationFilter: String
  var magnificationFilter: String
  var minificationFilterBias: Float
  var emitterCells: [CAEmitterCell]?
  var style: [NSObject : AnyObject]?
}

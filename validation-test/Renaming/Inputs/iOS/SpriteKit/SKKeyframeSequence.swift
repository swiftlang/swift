
enum SKInterpolationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linear
  case spline
  case step
}
enum SKRepeatMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case clamp
  case loop
}
class SKKeyframeSequence : NSObject, NSCoding, NSCopying {
  init(keyframeValues values: [AnyObject], times times: [NSNumber])
  convenience init(capacity numItems: Int)
  @discardableResult
  func count() -> Int
  func addKeyframeValue(_ value: AnyObject, time time: CGFloat)
  func removeLastKeyframe()
  func removeKeyframe(at index: Int)
  func setKeyframeValue(_ value: AnyObject, for index: Int)
  func setKeyframeTime(_ time: CGFloat, for index: Int)
  func setKeyframeValue(_ value: AnyObject, time time: CGFloat, for index: Int)
  @discardableResult
  func getKeyframeValue(for index: Int) -> AnyObject
  @discardableResult
  func getKeyframeTime(for index: Int) -> CGFloat
  @discardableResult
  func sample(atTime time: CGFloat) -> AnyObject?
  var interpolationMode: SKInterpolationMode
  var repeatMode: SKRepeatMode
}

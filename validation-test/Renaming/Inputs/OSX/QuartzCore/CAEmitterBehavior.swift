
class CAEmitterBehavior : NSObject, NSCoding {
  @discardableResult
  class func behaviorTypes() -> [String]
  init(type type: String)
  var type: String { get }
  var name: String?
  var isEnabled: Bool
  @discardableResult
  func inputKeys() -> [AnyObject]
  @discardableResult
  class func attributes(forKey key: String) -> [NSObject : AnyObject]
  @discardableResult
  func attributes(forKeyPath keyPath: String) -> [NSObject : AnyObject]
}
let kCAEmitterBehaviorWave: String
let kCAEmitterBehaviorDrag: String
let kCAEmitterBehaviorAlignToMotion: String
let kCAEmitterBehaviorValueOverLife: String
let kCAEmitterBehaviorColorOverLife: String
let kCAEmitterBehaviorLight: String
let kCAEmitterBehaviorAttractor: String

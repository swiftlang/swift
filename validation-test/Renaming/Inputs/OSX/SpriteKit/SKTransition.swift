
@available(OSX 10.9, *)
enum SKTransitionDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case up
  case down
  case right
  case left
}
class SKTransition : NSObject, NSCopying {
  @discardableResult
  class func crossFade(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func fade(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func fade(with color: NSColor, duration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func flipHorizontal(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func flipVertical(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func reveal(with direction: SKTransitionDirection, duration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func moveIn(with direction: SKTransitionDirection, duration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func push(with direction: SKTransitionDirection, duration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func doorsOpenHorizontal(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func doorsOpenVertical(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func doorsCloseHorizontal(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func doorsCloseVertical(withDuration sec: NSTimeInterval) -> SKTransition
  @discardableResult
  class func doorway(withDuration sec: NSTimeInterval) -> SKTransition
  /*not inherited*/ init(ciFilter filter: CIFilter, duration sec: NSTimeInterval)
  var pausesIncomingScene: Bool
  var pausesOutgoingScene: Bool
}

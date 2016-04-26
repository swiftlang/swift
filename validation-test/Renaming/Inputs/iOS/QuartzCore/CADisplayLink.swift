
class CADisplayLink : NSObject {
  /*not inherited*/ init(target target: AnyObject, selector sel: Selector)
  func add(to runloop: NSRunLoop, forMode mode: String)
  func remove(from runloop: NSRunLoop, forMode mode: String)
  func invalidate()
  var timestamp: CFTimeInterval { get }
  var duration: CFTimeInterval { get }
  var isPaused: Bool
  var frameInterval: Int
}

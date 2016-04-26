
class CARenderer : NSObject {
  /*not inherited*/ init(cglContext ctx: UnsafeMutablePointer<Void>, options dict: [NSObject : AnyObject]? = [:])
  var layer: CALayer?
  var bounds: CGRect
  func beginFrame(atTime t: CFTimeInterval, timeStamp ts: UnsafeMutablePointer<CVTimeStamp>?)
  @discardableResult
  func updateBounds() -> CGRect
  func addUpdate(_ r: CGRect)
  func render()
  @discardableResult
  func nextFrameTime() -> CFTimeInterval
  func endFrame()
}


class NSShadow : NSObject, NSCopying, NSCoding {
  var shadowOffset: NSSize
  var shadowBlurRadius: CGFloat
  @NSCopying var shadowColor: NSColor?
  func set()
}

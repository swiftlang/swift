
@available(iOS 6.0, *)
class NSShadow : NSObject, NSCopying, NSCoding {
  var shadowOffset: CGSize
  var shadowBlurRadius: CGFloat
  var shadowColor: AnyObject?
}

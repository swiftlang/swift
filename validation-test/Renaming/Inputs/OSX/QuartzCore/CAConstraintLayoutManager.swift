
enum CAConstraintAttribute : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case minX
  case midX
  case maxX
  case width
  case minY
  case midY
  case maxY
  case height
}
extension CALayer {
  var constraints: [CAConstraint]?
  func addConstraint(_ c: CAConstraint)
}
class CAConstraintLayoutManager : NSObject {
}
class CAConstraint : NSObject, NSCoding {
  convenience init(attribute attr: CAConstraintAttribute, relativeTo srcId: String, attribute srcAttr: CAConstraintAttribute, offset c: CGFloat)
  convenience init(attribute attr: CAConstraintAttribute, relativeTo srcId: String, attribute srcAttr: CAConstraintAttribute)
  init(attribute attr: CAConstraintAttribute, relativeTo srcId: String, attribute srcAttr: CAConstraintAttribute, scale m: CGFloat, offset c: CGFloat)
  var attribute: CAConstraintAttribute { get }
  var sourceName: String { get }
  var sourceAttribute: CAConstraintAttribute { get }
  var scale: CGFloat { get }
  var offset: CGFloat { get }
}

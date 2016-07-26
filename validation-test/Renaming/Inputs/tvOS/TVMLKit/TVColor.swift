
@available(tvOS 9.0, *)
enum TVColorType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case plain
  case linearGradientTopToBottom
  case linearGradientLeftToRight
}
@available(tvOS 9.0, *)
class TVColor : NSObject, NSCopying {
  var colorType: TVColorType { get }
  var color: UIColor? { get }
  var gradientColors: [UIColor]? { get }
  var gradientPoints: [NSNumber]? { get }
}

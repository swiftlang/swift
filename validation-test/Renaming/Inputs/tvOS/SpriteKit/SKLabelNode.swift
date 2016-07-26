
@available(tvOS 7.0, *)
enum SKLabelVerticalAlignmentMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case baseline
  case center
  case top
  case bottom
}
@available(tvOS 7.0, *)
enum SKLabelHorizontalAlignmentMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case center
  case left
  case right
}
class SKLabelNode : SKNode {
  convenience init(text text: String?)
  init(fontNamed fontName: String?)
  var verticalAlignmentMode: SKLabelVerticalAlignmentMode
  var horizontalAlignmentMode: SKLabelHorizontalAlignmentMode
  var fontName: String?
  var text: String?
  var fontSize: CGFloat
  var fontColor: UIColor?
  var colorBlendFactor: CGFloat
  var color: UIColor?
  var blendMode: SKBlendMode
}

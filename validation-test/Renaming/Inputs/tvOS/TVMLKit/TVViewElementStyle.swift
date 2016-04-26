
@available(tvOS 9.0, *)
enum TVElementAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case undefined
  case left
  case center
  case right
}
@available(tvOS 9.0, *)
enum TVElementContentAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case undefined
  case top
  case center
  case bottom
}
@available(tvOS 9.0, *)
enum TVElementPosition : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case undefined
  case center
  case top
  case bottom
  case left
  case right
  case topLeft
  case topRight
  case bottomLeft
  case bottomRight
  case header
  case footer
}
@available(tvOS 9.0, *)
class TVViewElementStyle : NSObject, NSCopying {
  @discardableResult
  func value(propertyName name: String) -> AnyObject?
  var backgroundColor: TVColor? { get }
  var color: TVColor? { get }
  var fontSize: CGFloat { get }
  var fontWeight: String? { get }
  var height: CGFloat { get }
  var margin: UIEdgeInsets { get }
  var maxHeight: CGFloat { get }
  var maxWidth: CGFloat { get }
  var minHeight: CGFloat { get }
  var minWidth: CGFloat { get }
  var padding: UIEdgeInsets { get }
  var textAlignment: NSTextAlignment { get }
  var width: CGFloat { get }
  var alignment: TVElementAlignment { get }
  var contentAlignment: TVElementContentAlignment { get }
  var highlightColor: TVColor? { get }
  var imageTreatmentName: String? { get }
  var interitemSpacing: CGFloat { get }
  var textHighlightStyle: String? { get }
  var textMinimumScaleFactor: CGFloat { get }
  var position: TVElementPosition { get }
  var ratingStyle: String? { get }
  var maxTextLines: Int { get }
  var textStyle: String? { get }
  var tintColor: TVColor? { get }
}

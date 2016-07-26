
@available(tvOS 9.0, *)
enum TVTextElementStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case title
  case subtitle
  case description
  case decoration
}
@available(tvOS 9.0, *)
class TVTextElement : TVViewElement {
  var attributedString: NSAttributedString? { get }
  var textStyle: TVTextElementStyle { get }
  @discardableResult
  func makeAttributedString(font font: UIFont) -> NSAttributedString
  @discardableResult
  func makeAttributedString(font font: UIFont, foregroundColor foregroundColor: UIColor?, textAlignment alignment: NSTextAlignment) -> NSAttributedString
}

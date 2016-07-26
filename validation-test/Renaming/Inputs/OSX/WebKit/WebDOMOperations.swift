
extension DOMNode {
  var webArchive: WebArchive! { get }
}
extension DOMDocument {
  @discardableResult
  func url(withAttributeString string: String!) -> NSURL!
}
extension DOMRange {
  var webArchive: WebArchive! { get }
  var markupString: String! { get }
}
extension DOMHTMLFrameElement {
  var contentFrame: WebFrame! { get }
}
extension DOMHTMLIFrameElement {
  var contentFrame: WebFrame! { get }
}
extension DOMHTMLObjectElement {
  var contentFrame: WebFrame! { get }
}

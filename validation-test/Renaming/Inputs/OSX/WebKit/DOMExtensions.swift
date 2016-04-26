
extension DOMNode {
  @available(OSX 10.5, *)
  @discardableResult
  func boundingBox() -> NSRect
  @available(OSX 10.5, *)
  @discardableResult
  func lineBoxRects() -> [AnyObject]!
}
extension DOMElement {
  @available(OSX 10.5, *)
  @discardableResult
  func image() -> NSImage!
}
extension DOMHTMLDocument {
  @available(OSX 10.5, *)
  @discardableResult
  func createDocumentFragment(withMarkupString markupString: String!, baseURL baseURL: NSURL!) -> DOMDocumentFragment!
  @available(OSX 10.5, *)
  @discardableResult
  func createDocumentFragment(withText text: String!) -> DOMDocumentFragment!
}

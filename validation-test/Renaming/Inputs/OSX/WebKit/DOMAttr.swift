
@available(OSX 10.4, *)
class DOMAttr : DOMNode {
  var name: String! { get }
  var specified: Bool { get }
  var value: String!
  var ownerElement: DOMElement! { get }
  @available(OSX 10.5, *)
  var style: DOMCSSStyleDeclaration! { get }
}


@available(OSX 10.4, *)
class DOMHTMLElement : DOMElement {
  var idName: String!
  var title: String!
  var lang: String!
  var dir: String!
  var tabIndex: Int32
  @available(OSX 10.8, *)
  var accessKey: String!
  var outerText: String!
  var children: DOMHTMLCollection! { get }
  var contentEditable: String!
  @available(OSX 10.5, *)
  var titleDisplayString: String! { get }
  @available(OSX 10.8, *)
  func click()
}

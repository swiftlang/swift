
@available(OSX 10.4, *)
class DOMHTMLIFrameElement : DOMHTMLElement {
  var align: String!
  var frameBorder: String!
  var height: String!
  var longDesc: String!
  var marginHeight: String!
  var marginWidth: String!
  var name: String!
  var scrolling: String!
  var src: String!
  var width: String!
  var contentDocument: DOMDocument! { get }
  @available(OSX 10.6, *)
  var contentWindow: DOMAbstractView! { get }
}

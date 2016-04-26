
@available(OSX 10.4, *)
class DOMHTMLFrameElement : DOMHTMLElement {
  var frameBorder: String!
  var longDesc: String!
  var marginHeight: String!
  var marginWidth: String!
  var name: String!
  var noResize: Bool
  var scrolling: String!
  var src: String!
  var contentDocument: DOMDocument! { get }
  @available(OSX 10.5, *)
  var contentWindow: DOMAbstractView! { get }
  @available(OSX 10.5, *)
  var location: String!
  @available(OSX 10.5, *)
  var width: Int32 { get }
  @available(OSX 10.5, *)
  var height: Int32 { get }
}

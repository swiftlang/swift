
@available(OSX 10.4, *)
class DOMHTMLAnchorElement : DOMHTMLElement {
  var charset: String!
  var coords: String!
  var href: String!
  var hreflang: String!
  var name: String!
  var rel: String!
  var rev: String!
  var shape: String!
  var target: String!
  var type: String!
  @available(OSX 10.5, *)
  var hashName: String! { get }
  @available(OSX 10.5, *)
  var host: String! { get }
  @available(OSX 10.5, *)
  var hostname: String! { get }
  @available(OSX 10.5, *)
  var pathname: String! { get }
  @available(OSX 10.5, *)
  var port: String! { get }
  @available(OSX 10.5, *)
  var `protocol`: String! { get }
  @available(OSX 10.5, *)
  var search: String! { get }
  @available(OSX 10.5, *)
  var text: String! { get }
  @available(OSX 10.5, *)
  @NSCopying var absoluteLinkURL: NSURL! { get }
}

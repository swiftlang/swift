
@available(OSX 10.4, *)
class DOMHTMLLinkElement : DOMHTMLElement {
  var disabled: Bool
  var charset: String!
  var href: String!
  var hreflang: String!
  var media: String!
  var rel: String!
  var rev: String!
  var target: String!
  var type: String!
  @available(OSX 10.5, *)
  @NSCopying var absoluteLinkURL: NSURL! { get }
}

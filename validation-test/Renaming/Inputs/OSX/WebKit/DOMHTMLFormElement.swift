
@available(OSX 10.4, *)
class DOMHTMLFormElement : DOMHTMLElement {
  var acceptCharset: String!
  var action: String!
  var enctype: String!
  @available(OSX 10.5, *)
  var encoding: String!
  var method: String!
  var name: String!
  var target: String!
  var elements: DOMHTMLCollection! { get }
  var length: Int32 { get }
  func submit()
  func reset()
}


@available(OSX 10.4, *)
class DOMHTMLButtonElement : DOMHTMLElement {
  @available(OSX 10.6, *)
  var autofocus: Bool
  var disabled: Bool
  var form: DOMHTMLFormElement! { get }
  var name: String!
  var type: String!
  var value: String!
  @available(OSX 10.6, *)
  var willValidate: Bool { get }
}


@available(OSX 10.4, *)
class DOMHTMLOptionElement : DOMHTMLElement {
  var disabled: Bool
  var form: DOMHTMLFormElement! { get }
  var label: String!
  var defaultSelected: Bool
  var selected: Bool
  var value: String!
  var text: String! { get }
  var index: Int32 { get }
}

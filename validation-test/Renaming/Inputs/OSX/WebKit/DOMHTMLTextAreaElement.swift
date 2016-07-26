
@available(OSX 10.4, *)
class DOMHTMLTextAreaElement : DOMHTMLElement {
  @available(OSX 10.6, *)
  var autofocus: Bool
  var cols: Int32
  var disabled: Bool
  var form: DOMHTMLFormElement! { get }
  var name: String!
  var readOnly: Bool
  var rows: Int32
  var type: String! { get }
  var defaultValue: String!
  var value: String!
  @available(OSX 10.6, *)
  var willValidate: Bool { get }
  @available(OSX 10.5, *)
  var selectionStart: Int32
  @available(OSX 10.5, *)
  var selectionEnd: Int32
  func select()
  @available(OSX 10.5, *)
  func setSelectionRange(_ start: Int32, end end: Int32)
}


@available(OSX 10.4, *)
class DOMHTMLInputElement : DOMHTMLElement {
  var accept: String!
  var alt: String!
  @available(OSX 10.6, *)
  var autofocus: Bool
  var defaultChecked: Bool
  var checked: Bool
  var disabled: Bool
  var form: DOMHTMLFormElement! { get }
  @available(OSX 10.6, *)
  var files: DOMFileList!
  @available(OSX 10.5, *)
  var indeterminate: Bool
  var maxLength: Int32
  @available(OSX 10.6, *)
  var multiple: Bool
  var name: String!
  var readOnly: Bool
  var size: String!
  var src: String!
  var type: String!
  var defaultValue: String!
  var value: String!
  @available(OSX 10.6, *)
  var willValidate: Bool { get }
  @available(OSX 10.5, *)
  var selectionStart: Int32
  @available(OSX 10.5, *)
  var selectionEnd: Int32
  var align: String!
  var useMap: String!
  @available(OSX 10.5, *)
  var altDisplayString: String! { get }
  @available(OSX 10.5, *)
  @NSCopying var absoluteImageURL: NSURL! { get }
  func select()
  @available(OSX 10.5, *)
  func setSelectionRange(_ start: Int32, end end: Int32)
}

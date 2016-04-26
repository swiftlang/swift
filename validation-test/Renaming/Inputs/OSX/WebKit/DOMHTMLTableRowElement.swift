
@available(OSX 10.4, *)
class DOMHTMLTableRowElement : DOMHTMLElement {
  var rowIndex: Int32 { get }
  var sectionRowIndex: Int32 { get }
  var cells: DOMHTMLCollection! { get }
  var align: String!
  var bgColor: String!
  var ch: String!
  var chOff: String!
  var vAlign: String!
  @discardableResult
  func insertCell(_ index: Int32) -> DOMHTMLElement!
  func deleteCell(_ index: Int32)
}

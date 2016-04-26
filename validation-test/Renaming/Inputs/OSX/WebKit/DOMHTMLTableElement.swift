
@available(OSX 10.4, *)
class DOMHTMLTableElement : DOMHTMLElement {
  var caption: DOMHTMLTableCaptionElement!
  var tHead: DOMHTMLTableSectionElement!
  var tFoot: DOMHTMLTableSectionElement!
  var rows: DOMHTMLCollection! { get }
  var tBodies: DOMHTMLCollection! { get }
  var align: String!
  var bgColor: String!
  var border: String!
  var cellPadding: String!
  var cellSpacing: String!
  var frameBorders: String!
  var rules: String!
  var summary: String!
  var width: String!
  @discardableResult
  func createTHead() -> DOMHTMLElement!
  func deleteTHead()
  @discardableResult
  func createTFoot() -> DOMHTMLElement!
  func deleteTFoot()
  @discardableResult
  func createCaption() -> DOMHTMLElement!
  func deleteCaption()
  @discardableResult
  func insertRow(_ index: Int32) -> DOMHTMLElement!
  func deleteRow(_ index: Int32)
}

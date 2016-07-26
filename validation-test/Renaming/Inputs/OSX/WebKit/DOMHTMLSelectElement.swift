
@available(OSX 10.4, *)
class DOMHTMLSelectElement : DOMHTMLElement {
  @available(OSX 10.6, *)
  var autofocus: Bool
  var disabled: Bool
  var form: DOMHTMLFormElement! { get }
  var multiple: Bool
  var name: String!
  var size: Int32
  var type: String! { get }
  var options: DOMHTMLOptionsCollection! { get }
  var length: Int32 { get }
  var selectedIndex: Int32
  var value: String!
  @available(OSX 10.6, *)
  var willValidate: Bool { get }
  @available(OSX 10.6, *)
  @discardableResult
  func item(_ index: UInt32) -> DOMNode!
  @available(OSX 10.6, *)
  @discardableResult
  func namedItem(_ name: String!) -> DOMNode!
  @available(OSX 10.5, *)
  func add(_ element: DOMHTMLElement!, before before: DOMHTMLElement!)
  func remove(_ index: Int32)
}
extension DOMHTMLSelectElement {
}

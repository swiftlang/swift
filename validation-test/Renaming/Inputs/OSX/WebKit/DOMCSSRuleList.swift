
@available(OSX 10.4, *)
class DOMCSSRuleList : DOMObject {
  var length: UInt32 { get }
  @discardableResult
  func item(_ index: UInt32) -> DOMCSSRule!
}

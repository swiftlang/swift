
@available(OSX 10.4, *)
class DOMCSSStyleSheet : DOMStyleSheet {
  var ownerRule: DOMCSSRule! { get }
  var cssRules: DOMCSSRuleList! { get }
  @available(OSX 10.6, *)
  var rules: DOMCSSRuleList! { get }
  @available(OSX 10.5, *)
  @discardableResult
  func insertRule(_ rule: String!, index index: UInt32) -> UInt32
  func deleteRule(_ index: UInt32)
  @available(OSX 10.6, *)
  @discardableResult
  func addRule(_ selector: String!, style style: String!, index index: UInt32) -> Int32
  @available(OSX 10.6, *)
  func removeRule(_ index: UInt32)
}
extension DOMCSSStyleSheet {
}

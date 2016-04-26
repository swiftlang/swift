
@available(OSX 10.4, *)
class DOMCSSMediaRule : DOMCSSRule {
  var media: DOMMediaList! { get }
  var cssRules: DOMCSSRuleList! { get }
  @available(OSX 10.5, *)
  @discardableResult
  func insert(_ rule: String!, index index: UInt32) -> UInt32
  func delete(_ index: UInt32)
}
extension DOMCSSMediaRule {
}

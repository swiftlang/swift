
@available(OSX 10.4, *)
class DOMCSSStyleRule : DOMCSSRule {
  var selectorText: String!
  var style: DOMCSSStyleDeclaration! { get }
}


@available(OSX 10.4, *)
class DOMCSSImportRule : DOMCSSRule {
  var href: String! { get }
  var media: DOMMediaList! { get }
  var styleSheet: DOMCSSStyleSheet! { get }
}

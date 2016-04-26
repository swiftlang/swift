
var DOM_CSS_INHERIT: Int { get }
var DOM_CSS_PRIMITIVE_VALUE: Int { get }
var DOM_CSS_VALUE_LIST: Int { get }
var DOM_CSS_CUSTOM: Int { get }
@available(OSX 10.4, *)
class DOMCSSValue : DOMObject {
  var cssText: String!
  var cssValueType: UInt16 { get }
}

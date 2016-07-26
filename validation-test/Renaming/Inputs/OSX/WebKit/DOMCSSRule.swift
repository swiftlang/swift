
var DOM_UNKNOWN_RULE: Int { get }
var DOM_STYLE_RULE: Int { get }
var DOM_CHARSET_RULE: Int { get }
var DOM_IMPORT_RULE: Int { get }
var DOM_MEDIA_RULE: Int { get }
var DOM_FONT_FACE_RULE: Int { get }
var DOM_PAGE_RULE: Int { get }
var DOM_KEYFRAMES_RULE: Int { get }
var DOM_KEYFRAME_RULE: Int { get }
var DOM_SUPPORTS_RULE: Int { get }
var DOM_WEBKIT_REGION_RULE: Int { get }
var DOM_WEBKIT_KEYFRAMES_RULE: Int { get }
var DOM_WEBKIT_KEYFRAME_RULE: Int { get }
@available(OSX 10.4, *)
class DOMCSSRule : DOMObject {
  var type: UInt16 { get }
  var cssText: String!
  var parentStyleSheet: DOMCSSStyleSheet! { get }
  var parent: DOMCSSRule! { get }
}

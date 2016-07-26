
var DOM_CSS_UNKNOWN: Int { get }
var DOM_CSS_NUMBER: Int { get }
var DOM_CSS_PERCENTAGE: Int { get }
var DOM_CSS_EMS: Int { get }
var DOM_CSS_EXS: Int { get }
var DOM_CSS_PX: Int { get }
var DOM_CSS_CM: Int { get }
var DOM_CSS_MM: Int { get }
var DOM_CSS_IN: Int { get }
var DOM_CSS_PT: Int { get }
var DOM_CSS_PC: Int { get }
var DOM_CSS_DEG: Int { get }
var DOM_CSS_RAD: Int { get }
var DOM_CSS_GRAD: Int { get }
var DOM_CSS_MS: Int { get }
var DOM_CSS_S: Int { get }
var DOM_CSS_HZ: Int { get }
var DOM_CSS_KHZ: Int { get }
var DOM_CSS_DIMENSION: Int { get }
var DOM_CSS_STRING: Int { get }
var DOM_CSS_URI: Int { get }
var DOM_CSS_IDENT: Int { get }
var DOM_CSS_ATTR: Int { get }
var DOM_CSS_COUNTER: Int { get }
var DOM_CSS_RECT: Int { get }
var DOM_CSS_RGBCOLOR: Int { get }
var DOM_CSS_VW: Int { get }
var DOM_CSS_VH: Int { get }
var DOM_CSS_VMIN: Int { get }
var DOM_CSS_VMAX: Int { get }
@available(OSX 10.4, *)
class DOMCSSPrimitiveValue : DOMCSSValue {
  var primitiveType: UInt16 { get }
  @available(OSX 10.5, *)
  func setFloat(_ unitType: UInt16, floatValue floatValue: Float)
  @discardableResult
  func getFloat(_ unitType: UInt16) -> Float
  @available(OSX 10.5, *)
  func setStringValue(_ stringType: UInt16, stringValue stringValue: String!)
  @discardableResult
  func getStringValue() -> String!
  @discardableResult
  func getCounterValue() -> DOMCounter!
  @discardableResult
  func getRectValue() -> DOMRect!
  @discardableResult
  func getRGBColorValue() -> DOMRGBColor!
}
extension DOMCSSPrimitiveValue {
}

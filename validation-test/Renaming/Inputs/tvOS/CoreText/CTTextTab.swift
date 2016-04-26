
class CTTextTab {
}
@available(tvOS 3.2, *)
@discardableResult
func CTTextTabGetTypeID() -> CFTypeID
@available(tvOS 3.2, *)
let kCTTabColumnTerminatorsAttributeName: CFString
@available(tvOS 3.2, *)
@discardableResult
func CTTextTabCreate(_ alignment: CTTextAlignment, _ location: Double, _ options: CFDictionary?) -> CTTextTab
@available(tvOS 3.2, *)
@discardableResult
func CTTextTabGetAlignment(_ tab: CTTextTab) -> CTTextAlignment
@available(tvOS 3.2, *)
@discardableResult
func CTTextTabGetLocation(_ tab: CTTextTab) -> Double
@available(tvOS 3.2, *)
@discardableResult
func CTTextTabGetOptions(_ tab: CTTextTab) -> CFDictionary?

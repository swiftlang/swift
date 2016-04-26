
class CTTextTab {
}
@available(watchOS 2.0, *)
@discardableResult
func CTTextTabGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
let kCTTabColumnTerminatorsAttributeName: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTTextTabCreate(_ alignment: CTTextAlignment, _ location: Double, _ options: CFDictionary?) -> CTTextTab
@available(watchOS 2.0, *)
@discardableResult
func CTTextTabGetAlignment(_ tab: CTTextTab) -> CTTextAlignment
@available(watchOS 2.0, *)
@discardableResult
func CTTextTabGetLocation(_ tab: CTTextTab) -> Double
@available(watchOS 2.0, *)
@discardableResult
func CTTextTabGetOptions(_ tab: CTTextTab) -> CFDictionary?

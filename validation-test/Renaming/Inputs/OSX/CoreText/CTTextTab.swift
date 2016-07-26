
class CTTextTab {
}
@available(OSX 10.5, *)
@discardableResult
func CTTextTabGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
let kCTTabColumnTerminatorsAttributeName: CFString
@available(OSX 10.5, *)
@discardableResult
func CTTextTabCreate(_ alignment: CTTextAlignment, _ location: Double, _ options: CFDictionary?) -> CTTextTab
@available(OSX 10.5, *)
@discardableResult
func CTTextTabGetAlignment(_ tab: CTTextTab) -> CTTextAlignment
@available(OSX 10.5, *)
@discardableResult
func CTTextTabGetLocation(_ tab: CTTextTab) -> Double
@available(OSX 10.5, *)
@discardableResult
func CTTextTabGetOptions(_ tab: CTTextTab) -> CFDictionary?

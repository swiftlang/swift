
class CTTypesetter {
}
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterGetTypeID() -> CFTypeID
@available(watchOS, introduced: 2.0, deprecated: 2.0)
let kCTTypesetterOptionDisableBidiProcessing: CFString
@available(watchOS 2.0, *)
let kCTTypesetterOptionForcedEmbeddingLevel: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTTypesetter
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterCreateWithAttributedStringAndOptions(_ string: CFAttributedString, _ options: CFDictionary?) -> CTTypesetter
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterCreateLineWithOffset(_ typesetter: CTTypesetter, _ stringRange: CFRange, _ offset: Double) -> CTLine
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterCreateLine(_ typesetter: CTTypesetter, _ stringRange: CFRange) -> CTLine
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterSuggestLineBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterSuggestLineBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterSuggestClusterBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func CTTypesetterSuggestClusterBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex


class CTTypesetter {
}
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
let kCTTypesetterOptionForcedEmbeddingLevel: CFString
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTTypesetter
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterCreateWithAttributedStringAndOptions(_ string: CFAttributedString, _ options: CFDictionary?) -> CTTypesetter
@available(OSX 10.6, *)
@discardableResult
func CTTypesetterCreateLineWithOffset(_ typesetter: CTTypesetter, _ stringRange: CFRange, _ offset: Double) -> CTLine
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterCreateLine(_ typesetter: CTTypesetter, _ stringRange: CFRange) -> CTLine
@available(OSX 10.6, *)
@discardableResult
func CTTypesetterSuggestLineBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterSuggestLineBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex
@available(OSX 10.6, *)
@discardableResult
func CTTypesetterSuggestClusterBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CTTypesetterSuggestClusterBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex

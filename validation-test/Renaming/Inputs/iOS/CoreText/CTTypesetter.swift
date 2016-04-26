
class CTTypesetter {
}
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterGetTypeID() -> CFTypeID
@available(iOS 3.2, *)
let kCTTypesetterOptionForcedEmbeddingLevel: CFString
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTTypesetter
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterCreateWithAttributedStringAndOptions(_ string: CFAttributedString, _ options: CFDictionary?) -> CTTypesetter
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterCreateLineWithOffset(_ typesetter: CTTypesetter, _ stringRange: CFRange, _ offset: Double) -> CTLine
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterCreateLine(_ typesetter: CTTypesetter, _ stringRange: CFRange) -> CTLine
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterSuggestLineBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterSuggestLineBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterSuggestClusterBreakWithOffset(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double, _ offset: Double) -> CFIndex
@available(iOS 3.2, *)
@discardableResult
func CTTypesetterSuggestClusterBreak(_ typesetter: CTTypesetter, _ startIndex: CFIndex, _ width: Double) -> CFIndex

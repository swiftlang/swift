
class CTFramesetter {
}
@available(watchOS 2.0, *)
@discardableResult
func CTFramesetterGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
@discardableResult
func CTFramesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTFramesetter
@available(watchOS 2.0, *)
@discardableResult
func CTFramesetterCreateFrame(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ path: CGPath, _ frameAttributes: CFDictionary?) -> CTFrame
@available(watchOS 2.0, *)
@discardableResult
func CTFramesetterGetTypesetter(_ framesetter: CTFramesetter) -> CTTypesetter
@available(watchOS 2.0, *)
@discardableResult
func CTFramesetterSuggestFrameSizeWithConstraints(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ frameAttributes: CFDictionary?, _ constraints: CGSize, _ fitRange: UnsafeMutablePointer<CFRange>?) -> CGSize

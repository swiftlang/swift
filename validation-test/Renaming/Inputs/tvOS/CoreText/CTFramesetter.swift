
class CTFramesetter {
}
@available(tvOS 3.2, *)
@discardableResult
func CTFramesetterGetTypeID() -> CFTypeID
@available(tvOS 3.2, *)
@discardableResult
func CTFramesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTFramesetter
@available(tvOS 3.2, *)
@discardableResult
func CTFramesetterCreateFrame(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ path: CGPath, _ frameAttributes: CFDictionary?) -> CTFrame
@available(tvOS 3.2, *)
@discardableResult
func CTFramesetterGetTypesetter(_ framesetter: CTFramesetter) -> CTTypesetter
@available(tvOS 3.2, *)
@discardableResult
func CTFramesetterSuggestFrameSizeWithConstraints(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ frameAttributes: CFDictionary?, _ constraints: CGSize, _ fitRange: UnsafeMutablePointer<CFRange>?) -> CGSize

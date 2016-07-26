
class CTFramesetter {
}
@available(iOS 3.2, *)
@discardableResult
func CTFramesetterGetTypeID() -> CFTypeID
@available(iOS 3.2, *)
@discardableResult
func CTFramesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTFramesetter
@available(iOS 3.2, *)
@discardableResult
func CTFramesetterCreateFrame(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ path: CGPath, _ frameAttributes: CFDictionary?) -> CTFrame
@available(iOS 3.2, *)
@discardableResult
func CTFramesetterGetTypesetter(_ framesetter: CTFramesetter) -> CTTypesetter
@available(iOS 3.2, *)
@discardableResult
func CTFramesetterSuggestFrameSizeWithConstraints(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ frameAttributes: CFDictionary?, _ constraints: CGSize, _ fitRange: UnsafeMutablePointer<CFRange>?) -> CGSize

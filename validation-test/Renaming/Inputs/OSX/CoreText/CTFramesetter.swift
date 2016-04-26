
class CTFramesetter {
}
@available(OSX 10.5, *)
@discardableResult
func CTFramesetterGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func CTFramesetterCreateWithAttributedString(_ string: CFAttributedString) -> CTFramesetter
@available(OSX 10.5, *)
@discardableResult
func CTFramesetterCreateFrame(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ path: CGPath, _ frameAttributes: CFDictionary?) -> CTFrame
@available(OSX 10.5, *)
@discardableResult
func CTFramesetterGetTypesetter(_ framesetter: CTFramesetter) -> CTTypesetter
@available(OSX 10.5, *)
@discardableResult
func CTFramesetterSuggestFrameSizeWithConstraints(_ framesetter: CTFramesetter, _ stringRange: CFRange, _ frameAttributes: CFDictionary?, _ constraints: CGSize, _ fitRange: UnsafeMutablePointer<CFRange>?) -> CGSize

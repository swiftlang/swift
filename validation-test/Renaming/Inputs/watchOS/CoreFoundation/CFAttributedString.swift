
class CFAttributedString {
}
class CFMutableAttributedString {
}
@discardableResult
func CFAttributedStringGetTypeID() -> CFTypeID
@discardableResult
func CFAttributedStringCreate(_ alloc: CFAllocator!, _ str: CFString!, _ attributes: CFDictionary!) -> CFAttributedString!
@discardableResult
func CFAttributedStringCreateWithSubstring(_ alloc: CFAllocator!, _ aStr: CFAttributedString!, _ range: CFRange) -> CFAttributedString!
@discardableResult
func CFAttributedStringCreateCopy(_ alloc: CFAllocator!, _ aStr: CFAttributedString!) -> CFAttributedString!
@discardableResult
func CFAttributedStringGetString(_ aStr: CFAttributedString!) -> CFString!
@discardableResult
func CFAttributedStringGetLength(_ aStr: CFAttributedString!) -> CFIndex
@discardableResult
func CFAttributedStringGetAttributes(_ aStr: CFAttributedString!, _ loc: CFIndex, _ effectiveRange: UnsafeMutablePointer<CFRange>!) -> CFDictionary!
@discardableResult
func CFAttributedStringGetAttribute(_ aStr: CFAttributedString!, _ loc: CFIndex, _ attrName: CFString!, _ effectiveRange: UnsafeMutablePointer<CFRange>!) -> CFTypeRef!
@discardableResult
func CFAttributedStringGetAttributesAndLongestEffectiveRange(_ aStr: CFAttributedString!, _ loc: CFIndex, _ inRange: CFRange, _ longestEffectiveRange: UnsafeMutablePointer<CFRange>!) -> CFDictionary!
@discardableResult
func CFAttributedStringGetAttributeAndLongestEffectiveRange(_ aStr: CFAttributedString!, _ loc: CFIndex, _ attrName: CFString!, _ inRange: CFRange, _ longestEffectiveRange: UnsafeMutablePointer<CFRange>!) -> CFTypeRef!
@discardableResult
func CFAttributedStringCreateMutableCopy(_ alloc: CFAllocator!, _ maxLength: CFIndex, _ aStr: CFAttributedString!) -> CFMutableAttributedString!
@discardableResult
func CFAttributedStringCreateMutable(_ alloc: CFAllocator!, _ maxLength: CFIndex) -> CFMutableAttributedString!
func CFAttributedStringReplaceString(_ aStr: CFMutableAttributedString!, _ range: CFRange, _ replacement: CFString!)
@discardableResult
func CFAttributedStringGetMutableString(_ aStr: CFMutableAttributedString!) -> CFMutableString!
func CFAttributedStringSetAttributes(_ aStr: CFMutableAttributedString!, _ range: CFRange, _ replacement: CFDictionary!, _ clearOtherAttributes: Bool)
func CFAttributedStringSetAttribute(_ aStr: CFMutableAttributedString!, _ range: CFRange, _ attrName: CFString!, _ value: CFTypeRef!)
func CFAttributedStringRemoveAttribute(_ aStr: CFMutableAttributedString!, _ range: CFRange, _ attrName: CFString!)
func CFAttributedStringReplaceAttributedString(_ aStr: CFMutableAttributedString!, _ range: CFRange, _ replacement: CFAttributedString!)
func CFAttributedStringBeginEditing(_ aStr: CFMutableAttributedString!)
func CFAttributedStringEndEditing(_ aStr: CFMutableAttributedString!)

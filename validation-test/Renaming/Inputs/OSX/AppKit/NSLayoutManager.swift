
@available(OSX 10.7, *)
enum NSTextLayoutOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case horizontal
  case vertical
}
@available(OSX 10.11, *)
enum NSGlyphProperty : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case null
  case controlCharacter
  case elastic
  case nonBaseCharacter
}
@available(OSX 10.11, *)
enum NSControlCharacterAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case zeroAdvancement
  case whitespace
  case horizontalTab
  case lineBreak
  case paragraphBreak
  case containerBreak
}
protocol NSTextLayoutOrientationProvider {
  @available(OSX 10.7, *)
  var layoutOrientation: NSTextLayoutOrientation { get }
}
enum NSTypesetterBehavior : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case latestBehavior
  case originalBehavior
  case behavior_10_2_WithCompatibility
  case behavior_10_2
  case behavior_10_3
  case behavior_10_4
}
@available(OSX 10.0, *)
class NSLayoutManager : NSObject, NSCoding {
  unowned(unsafe) var textStorage: @sil_unmanaged NSTextStorage?
  func replaceTextStorage(_ newTextStorage: NSTextStorage)
  var textContainers: [NSTextContainer] { get }
  func addTextContainer(_ container: NSTextContainer)
  func insertTextContainer(_ container: NSTextContainer, at index: Int)
  func removeTextContainer(at index: Int)
  func textContainerChangedGeometry(_ container: NSTextContainer)
  func textContainerChangedTextView(_ container: NSTextContainer)
  unowned(unsafe) var delegate: @sil_unmanaged NSLayoutManagerDelegate?
  var showsInvisibleCharacters: Bool
  var showsControlCharacters: Bool
  var hyphenationFactor: Float
  var usesFontLeading: Bool
  @available(OSX 10.5, *)
  var allowsNonContiguousLayout: Bool
  @available(OSX 10.5, *)
  var hasNonContiguousLayout: Bool { get }
  var backgroundLayoutEnabled: Bool
  var defaultAttachmentScaling: NSImageScaling
  var typesetter: NSTypesetter
  var typesetterBehavior: NSTypesetterBehavior
  func invalidateGlyphs(forCharacterRange charRange: NSRange, changeInLength delta: Int, actualCharacterRange actualCharRange: NSRangePointer?)
  @available(OSX 10.5, *)
  func invalidateLayout(forCharacterRange charRange: NSRange, actualCharacterRange actualCharRange: NSRangePointer?)
  func invalidateDisplay(forCharacterRange charRange: NSRange)
  func invalidateDisplay(forGlyphRange glyphRange: NSRange)
  @available(OSX 10.11, *)
  func processEditing(for textStorage: NSTextStorage, edited editMask: NSTextStorageEditActions, range newCharRange: NSRange, changeInLength delta: Int, invalidatedRange invalidatedCharRange: NSRange)
  func ensureGlyphs(forCharacterRange charRange: NSRange)
  func ensureGlyphs(forGlyphRange glyphRange: NSRange)
  func ensureLayout(forCharacterRange charRange: NSRange)
  func ensureLayout(forGlyphRange glyphRange: NSRange)
  func ensureLayout(for container: NSTextContainer)
  func ensureLayout(forBoundingRect bounds: NSRect, in container: NSTextContainer)
  @available(OSX 10.11, *)
  func setGlyphs(_ glyphs: UnsafePointer<CGGlyph>, properties props: UnsafePointer<NSGlyphProperty>, characterIndexes charIndexes: UnsafePointer<Int>, font aFont: NSFont, forGlyphRange glyphRange: NSRange)
  var numberOfGlyphs: Int { get }
  @discardableResult
  func cgGlyph(at glyphIndex: Int, isValidIndex isValidIndex: UnsafeMutablePointer<ObjCBool>?) -> CGGlyph
  @discardableResult
  func cgGlyph(at glyphIndex: Int) -> CGGlyph
  @discardableResult
  func isValidGlyphIndex(_ glyphIndex: Int) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func propertyForGlyph(at glyphIndex: Int) -> NSGlyphProperty
  @discardableResult
  func characterIndexForGlyph(at glyphIndex: Int) -> Int
  @discardableResult
  func glyphIndexForCharacter(at charIndex: Int) -> Int
  @available(OSX 10.5, *)
  @discardableResult
  func getGlyphsIn(_ glyphRange: NSRange, glyphs glyphBuffer: UnsafeMutablePointer<CGGlyph>?, properties props: UnsafeMutablePointer<NSGlyphProperty>?, characterIndexes charIndexBuffer: UnsafeMutablePointer<Int>?, bidiLevels bidiLevelBuffer: UnsafeMutablePointer<UInt8>?) -> Int
  func setTextContainer(_ container: NSTextContainer, forGlyphRange glyphRange: NSRange)
  func setLineFragmentRect(_ fragmentRect: NSRect, forGlyphRange glyphRange: NSRange, usedRect usedRect: NSRect)
  func setExtraLineFragmentRect(_ fragmentRect: NSRect, usedRect usedRect: NSRect, textContainer container: NSTextContainer)
  func setLocation(_ location: NSPoint, forStartOfGlyphRange glyphRange: NSRange)
  func setNotShownAttribute(_ flag: Bool, forGlyphAt glyphIndex: Int)
  func setDrawsOutsideLineFragment(_ flag: Bool, forGlyphAt glyphIndex: Int)
  func setAttachmentSize(_ attachmentSize: NSSize, forGlyphRange glyphRange: NSRange)
  func getFirstUnlaidCharacterIndex(_ charIndex: UnsafeMutablePointer<Int>?, glyphIndex glyphIndex: UnsafeMutablePointer<Int>?)
  @discardableResult
  func firstUnlaidCharacterIndex() -> Int
  @discardableResult
  func firstUnlaidGlyphIndex() -> Int
  @discardableResult
  func textContainerForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?) -> NSTextContainer?
  @available(OSX 10.0, *)
  @discardableResult
  func textContainerForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?, withoutAdditionalLayout flag: Bool) -> NSTextContainer?
  @discardableResult
  func usedRect(for container: NSTextContainer) -> NSRect
  @discardableResult
  func lineFragmentRectForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?) -> NSRect
  @available(OSX 10.0, *)
  @discardableResult
  func lineFragmentRectForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?, withoutAdditionalLayout flag: Bool) -> NSRect
  @discardableResult
  func lineFragmentUsedRectForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?) -> NSRect
  @available(OSX 10.0, *)
  @discardableResult
  func lineFragmentUsedRectForGlyph(at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?, withoutAdditionalLayout flag: Bool) -> NSRect
  var extraLineFragmentRect: NSRect { get }
  var extraLineFragmentUsedRect: NSRect { get }
  var extraLineFragmentTextContainer: NSTextContainer? { get }
  @discardableResult
  func locationForGlyph(at glyphIndex: Int) -> NSPoint
  @discardableResult
  func notShownAttributeForGlyph(at glyphIndex: Int) -> Bool
  @discardableResult
  func drawsOutsideLineFragmentForGlyph(at glyphIndex: Int) -> Bool
  @discardableResult
  func attachmentSizeForGlyph(at glyphIndex: Int) -> NSSize
  @available(OSX 10.11, *)
  @discardableResult
  func truncatedGlyphRangeInLineFragmentForGlyph(at glyphIndex: Int) -> NSRange
  @discardableResult
  func glyphRange(forCharacterRange charRange: NSRange, actualCharacterRange actualCharRange: NSRangePointer?) -> NSRange
  @discardableResult
  func characterRange(forGlyphRange glyphRange: NSRange, actualGlyphRange actualGlyphRange: NSRangePointer?) -> NSRange
  @discardableResult
  func glyphRange(for container: NSTextContainer) -> NSRange
  @discardableResult
  func range(ofNominallySpacedGlyphsContaining glyphIndex: Int) -> NSRange
  @discardableResult
  func boundingRect(forGlyphRange glyphRange: NSRange, in container: NSTextContainer) -> NSRect
  @discardableResult
  func glyphRange(forBoundingRect bounds: NSRect, in container: NSTextContainer) -> NSRange
  @discardableResult
  func glyphRange(forBoundingRectWithoutAdditionalLayout bounds: NSRect, in container: NSTextContainer) -> NSRange
  @discardableResult
  func glyphIndex(for point: NSPoint, in container: NSTextContainer, fractionOfDistanceThroughGlyph partialFraction: UnsafeMutablePointer<CGFloat>?) -> Int
  @discardableResult
  func glyphIndex(for point: NSPoint, in container: NSTextContainer) -> Int
  @discardableResult
  func fractionOfDistanceThroughGlyph(for point: NSPoint, in container: NSTextContainer) -> CGFloat
  @discardableResult
  func characterIndex(for point: NSPoint, in container: NSTextContainer, fractionOfDistanceBetweenInsertionPoints partialFraction: UnsafeMutablePointer<CGFloat>?) -> Int
  @discardableResult
  func getLineFragmentInsertionPointsForCharacter(at charIndex: Int, alternatePositions aFlag: Bool, inDisplayOrder dFlag: Bool, positions positions: UnsafeMutablePointer<CGFloat>?, characterIndexes charIndexes: UnsafeMutablePointer<Int>?) -> Int
  @available(OSX 10.11, *)
  func enumerateLineFragments(forGlyphRange glyphRange: NSRange, using block: (NSRect, NSRect, NSTextContainer, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.11, *)
  func enumerateEnclosingRects(forGlyphRange glyphRange: NSRange, withinSelectedGlyphRange selectedRange: NSRange, in textContainer: NSTextContainer, using block: (NSRect, UnsafeMutablePointer<ObjCBool>) -> Void)
  func drawBackground(forGlyphRange glyphsToShow: NSRange, at origin: NSPoint)
  func drawGlyphs(forGlyphRange glyphsToShow: NSRange, at origin: NSPoint)
  @available(OSX 10.7, *)
  func showCGGlyphs(_ glyphs: UnsafePointer<CGGlyph>, positions positions: UnsafePointer<NSPoint>, count glyphCount: Int, font font: NSFont, matrix textMatrix: NSAffineTransform, attributes attributes: [String : AnyObject] = [:], in graphicsContext: NSGraphicsContext)
  @available(OSX 10.6, *)
  func fillBackgroundRectArray(_ rectArray: UnsafePointer<NSRect>, count rectCount: Int, forCharacterRange charRange: NSRange, color color: NSColor)
  func drawUnderline(forGlyphRange glyphRange: NSRange, underlineType underlineVal: NSUnderlineStyle, baselineOffset baselineOffset: CGFloat, lineFragmentRect lineRect: NSRect, lineFragmentGlyphRange lineGlyphRange: NSRange, containerOrigin containerOrigin: NSPoint)
  func underlineGlyphRange(_ glyphRange: NSRange, underlineType underlineVal: NSUnderlineStyle, lineFragmentRect lineRect: NSRect, lineFragmentGlyphRange lineGlyphRange: NSRange, containerOrigin containerOrigin: NSPoint)
  func drawStrikethrough(forGlyphRange glyphRange: NSRange, strikethroughType strikethroughVal: NSUnderlineStyle, baselineOffset baselineOffset: CGFloat, lineFragmentRect lineRect: NSRect, lineFragmentGlyphRange lineGlyphRange: NSRange, containerOrigin containerOrigin: NSPoint)
  func strikethroughGlyphRange(_ glyphRange: NSRange, strikethroughType strikethroughVal: NSUnderlineStyle, lineFragmentRect lineRect: NSRect, lineFragmentGlyphRange lineGlyphRange: NSRange, containerOrigin containerOrigin: NSPoint)
  func showAttachmentCell(_ cell: NSCell, in rect: NSRect, characterIndex attachmentIndex: Int)
  func setLayoutRect(_ rect: NSRect, for block: NSTextBlock, glyphRange glyphRange: NSRange)
  func setBoundsRect(_ rect: NSRect, for block: NSTextBlock, glyphRange glyphRange: NSRange)
  @discardableResult
  func layoutRect(for block: NSTextBlock, glyphRange glyphRange: NSRange) -> NSRect
  @discardableResult
  func boundsRect(for block: NSTextBlock, glyphRange glyphRange: NSRange) -> NSRect
  @discardableResult
  func layoutRect(for block: NSTextBlock, at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?) -> NSRect
  @discardableResult
  func boundsRect(for block: NSTextBlock, at glyphIndex: Int, effectiveRange effectiveGlyphRange: NSRangePointer?) -> NSRect
  @discardableResult
  func temporaryAttributes(atCharacterIndex charIndex: Int, effectiveRange effectiveCharRange: NSRangePointer?) -> [String : AnyObject]
  func setTemporaryAttributes(_ attrs: [String : AnyObject], forCharacterRange charRange: NSRange)
  func addTemporaryAttributes(_ attrs: [String : AnyObject] = [:], forCharacterRange charRange: NSRange)
  func removeTemporaryAttribute(_ attrName: String, forCharacterRange charRange: NSRange)
  @available(OSX 10.5, *)
  @discardableResult
  func temporaryAttribute(_ attrName: String, atCharacterIndex location: Int, effectiveRange range: NSRangePointer?) -> AnyObject?
  @available(OSX 10.5, *)
  @discardableResult
  func temporaryAttribute(_ attrName: String, atCharacterIndex location: Int, longestEffectiveRange range: NSRangePointer?, in rangeLimit: NSRange) -> AnyObject?
  @available(OSX 10.5, *)
  @discardableResult
  func temporaryAttributes(atCharacterIndex location: Int, longestEffectiveRange range: NSRangePointer?, in rangeLimit: NSRange) -> [String : AnyObject]
  @available(OSX 10.5, *)
  func addTemporaryAttribute(_ attrName: String, value value: AnyObject, forCharacterRange charRange: NSRange)
  @discardableResult
  func defaultLineHeight(for theFont: NSFont) -> CGFloat
  @discardableResult
  func defaultBaselineOffset(for theFont: NSFont) -> CGFloat
}
struct __lmFlags {
  var containersAreFull: UInt32
  var glyphsMightDrawOutsideLines: UInt32
  var backgroundLayoutEnabled: UInt32
  var resizingInProgress: UInt32
  var allowScreenFonts: UInt32
  var cachedRectArrayInUse: UInt32
  var displayInvalidationInProgress: UInt32
  var insertionPointNeedsUpdate: UInt32
  var layoutManagerInDirtyList: UInt32
  var originalFontOverride: UInt32
  var showInvisibleCharacters: UInt32
  var showControlCharacters: UInt32
  var delegateRespondsToDidInvalidate: UInt32
  var delegateRespondsToDidComplete: UInt32
  var glyphFormat: UInt32
  var textStorageRespondsToIsEditing: UInt32
  var notifyEditedInProgress: UInt32
  var containersChanged: UInt32
  var isGeneratingGlyphs: UInt32
  var hasNonGeneratedGlyphData: UInt32
  var inBackgroundLayout: UInt32
  var syncAlignmentToDirection: UInt32
  var defaultAttachmentScaling: UInt32
  var usesFontLeading: UInt32
  var seenRightToLeft: UInt32
  var ignoresViewTransformations: UInt32
  var needToFlushGlyph: UInt32
  var flipsIfNeeded: UInt32
  var allowNonContig: UInt32
  var useNonContig: UInt32
  init()
  init(containersAreFull containersAreFull: UInt32, glyphsMightDrawOutsideLines glyphsMightDrawOutsideLines: UInt32, backgroundLayoutEnabled backgroundLayoutEnabled: UInt32, resizingInProgress resizingInProgress: UInt32, allowScreenFonts allowScreenFonts: UInt32, cachedRectArrayInUse cachedRectArrayInUse: UInt32, displayInvalidationInProgress displayInvalidationInProgress: UInt32, insertionPointNeedsUpdate insertionPointNeedsUpdate: UInt32, layoutManagerInDirtyList layoutManagerInDirtyList: UInt32, originalFontOverride originalFontOverride: UInt32, showInvisibleCharacters showInvisibleCharacters: UInt32, showControlCharacters showControlCharacters: UInt32, delegateRespondsToDidInvalidate delegateRespondsToDidInvalidate: UInt32, delegateRespondsToDidComplete delegateRespondsToDidComplete: UInt32, glyphFormat glyphFormat: UInt32, textStorageRespondsToIsEditing textStorageRespondsToIsEditing: UInt32, notifyEditedInProgress notifyEditedInProgress: UInt32, containersChanged containersChanged: UInt32, isGeneratingGlyphs isGeneratingGlyphs: UInt32, hasNonGeneratedGlyphData hasNonGeneratedGlyphData: UInt32, inBackgroundLayout inBackgroundLayout: UInt32, syncAlignmentToDirection syncAlignmentToDirection: UInt32, defaultAttachmentScaling defaultAttachmentScaling: UInt32, usesFontLeading usesFontLeading: UInt32, seenRightToLeft seenRightToLeft: UInt32, ignoresViewTransformations ignoresViewTransformations: UInt32, needToFlushGlyph needToFlushGlyph: UInt32, flipsIfNeeded flipsIfNeeded: UInt32, allowNonContig allowNonContig: UInt32, useNonContig useNonContig: UInt32)
}
extension NSLayoutManager : NSGlyphStorage {
  var glyphGenerator: NSGlyphGenerator
}
extension NSLayoutManager {
  @discardableResult
  func rulerMarkers(for view: NSTextView, paragraphStyle style: NSParagraphStyle, ruler ruler: NSRulerView) -> [NSRulerMarker]
  @discardableResult
  func rulerAccessoryView(for view: NSTextView, paragraphStyle style: NSParagraphStyle, ruler ruler: NSRulerView, enabled isEnabled: Bool) -> NSView?
  @discardableResult
  func layoutManagerOwnsFirstResponder(in window: NSWindow) -> Bool
  unowned(unsafe) var firstTextView: @sil_unmanaged NSTextView? { get }
  unowned(unsafe) var textViewForBeginningOfSelection: @sil_unmanaged NSTextView? { get }
}
protocol NSLayoutManagerDelegate : NSObjectProtocol {
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldGenerateGlyphs glyphs: UnsafePointer<CGGlyph>, properties props: UnsafePointer<NSGlyphProperty>, characterIndexes charIndexes: UnsafePointer<Int>, font aFont: NSFont, forGlyphRange glyphRange: NSRange) -> Int
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, lineSpacingAfterGlyphAt glyphIndex: Int, withProposedLineFragmentRect rect: NSRect) -> CGFloat
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, paragraphSpacingBeforeGlyphAt glyphIndex: Int, withProposedLineFragmentRect rect: NSRect) -> CGFloat
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, paragraphSpacingAfterGlyphAt glyphIndex: Int, withProposedLineFragmentRect rect: NSRect) -> CGFloat
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldUse action: NSControlCharacterAction, forControlCharacterAt charIndex: Int) -> NSControlCharacterAction
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldBreakLineByWordBeforeCharacterAt charIndex: Int) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldBreakLineByHyphenatingBeforeCharacterAt charIndex: Int) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, boundingBoxForControlGlyphAt glyphIndex: Int, for textContainer: NSTextContainer, proposedLineFragment proposedRect: NSRect, glyphPosition glyphPosition: NSPoint, characterIndex charIndex: Int) -> NSRect
  @available(OSX 10.11, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldSetLineFragmentRect lineFragmentRect: UnsafeMutablePointer<NSRect>, lineFragmentUsedRect lineFragmentUsedRect: UnsafeMutablePointer<NSRect>, baselineOffset baselineOffset: UnsafeMutablePointer<CGFloat>, in textContainer: NSTextContainer, forGlyphRange glyphRange: NSRange) -> Bool
  @available(OSX 10.0, *)
  optional func layoutManagerDidInvalidateLayout(_ sender: NSLayoutManager)
  @available(OSX 10.0, *)
  optional func layoutManager(_ layoutManager: NSLayoutManager, didCompleteLayoutFor textContainer: NSTextContainer?, atEnd layoutFinishedFlag: Bool)
  @available(OSX 10.11, *)
  optional func layoutManager(_ layoutManager: NSLayoutManager, textContainer textContainer: NSTextContainer, didChangeGeometryFrom oldSize: NSSize)
  @available(OSX 10.5, *)
  @discardableResult
  optional func layoutManager(_ layoutManager: NSLayoutManager, shouldUseTemporaryAttributes attrs: [String : AnyObject] = [:], forDrawingToScreen toScreen: Bool, atCharacterIndex charIndex: Int, effectiveRange effectiveCharRange: NSRangePointer?) -> [String : AnyObject]?
}
var NSGlyphAttributeSoft: Int { get }
var NSGlyphAttributeElastic: Int { get }
var NSGlyphAttributeBidiLevel: Int { get }
var NSGlyphAttributeInscribe: Int { get }
@available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use NSGlyphProperty instead")
enum NSGlyphInscription : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case inscribeBase
  case inscribeBelow
  case inscribeAbove
  case inscribeOverstrike
  case inscribeOverBelow
}
extension NSLayoutManager {
  @discardableResult
  func glyph(at glyphIndex: Int, isValidIndex isValidIndex: UnsafeMutablePointer<ObjCBool>?) -> NSGlyph
  @discardableResult
  func glyph(at glyphIndex: Int) -> NSGlyph
  @discardableResult
  func rectArray(forCharacterRange charRange: NSRange, withinSelectedCharacterRange selCharRange: NSRange, in container: NSTextContainer, rectCount rectCount: UnsafeMutablePointer<Int>) -> NSRectArray?
  @discardableResult
  func rectArray(forGlyphRange glyphRange: NSRange, withinSelectedGlyphRange selGlyphRange: NSRange, in container: NSTextContainer, rectCount rectCount: UnsafeMutablePointer<Int>) -> NSRectArray?
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  var usesScreenFonts: Bool
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  @discardableResult
  func substituteFont(for originalFont: NSFont) -> NSFont
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -setGlyphs:properties:characterIndexes:font:forGlyphRange instead")
  func insertGlyph(_ glyph: NSGlyph, atGlyphIndex glyphIndex: Int, characterIndex charIndex: Int)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -setGlyphs:properties:characterIndexes:font:forGlyphRange instead")
  func replaceGlyph(at glyphIndex: Int, withGlyph newGlyph: NSGlyph)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -setGlyphs:properties:characterIndexes:font:forGlyphRange instead")
  func deleteGlyphs(in glyphRange: NSRange)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -setGlyphs:properties:characterIndexes:font:forGlyphRange instead")
  func setCharacterIndex(_ charIndex: Int, forGlyphAt glyphIndex: Int)
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use -setGlyphs:properties:characterIndexes:font:forGlyphRange instead")
  func invalidateGlyphsOnLayoutInvalidation(forGlyphRange glyphRange: NSRange)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels: instead")
  @discardableResult
  func intAttribute(_ attributeTag: Int, forGlyphAt glyphIndex: Int) -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels: instead")
  @discardableResult
  func getGlyphsIn(_ glyphRange: NSRange, glyphs glyphBuffer: UnsafeMutablePointer<NSGlyph>?, characterIndexes charIndexBuffer: UnsafeMutablePointer<Int>?, glyphInscriptions inscribeBuffer: UnsafeMutablePointer<NSGlyphInscription>?, elasticBits elasticBuffer: UnsafeMutablePointer<ObjCBool>?) -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels: instead")
  @discardableResult
  func getGlyphsIn(_ glyphRange: NSRange, glyphs glyphBuffer: UnsafeMutablePointer<NSGlyph>?, characterIndexes charIndexBuffer: UnsafeMutablePointer<Int>?, glyphInscriptions inscribeBuffer: UnsafeMutablePointer<NSGlyphInscription>?, elasticBits elasticBuffer: UnsafeMutablePointer<ObjCBool>?, bidiLevels bidiLevelBuffer: UnsafeMutablePointer<UInt8>?) -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -getGlyphsInRange:glyphs:properties:characterIndexes:bidiLevels: instead")
  @discardableResult
  func getGlyphs(_ glyphArray: UnsafeMutablePointer<NSGlyph>?, range glyphRange: NSRange) -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -invalidateLayoutForCharacterRange:actualCharacterRange: instead")
  func invalidateLayout(forCharacterRange charRange: NSRange, isSoft flag: Bool, actualCharacterRange actualCharRange: NSRangePointer?)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -processEditingForTextStorage:edited:range:changeInLength:invalidatedRange: instead")
  func textStorage(_ str: NSTextStorage, edited editedMask: NSTextStorageEditedOptions, range newCharRange: NSRange, changeInLength delta: Int, invalidatedRange invalidatedCharRange: NSRange)
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use -setLocation:forStartOfGlyphRange: instead")
  func setLocations(_ locations: NSPointArray, startingGlyphIndexes glyphIndexes: UnsafeMutablePointer<Int>, count count: Int, forGlyphRange glyphRange: NSRange)
}

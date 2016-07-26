
class NSMenuItemCell : NSButtonCell {
  var menuItem: NSMenuItem?
  var needsSizing: Bool
  func calcSize()
  var needsDisplay: Bool
  var stateImageWidth: CGFloat { get }
  var imageWidth: CGFloat { get }
  var titleWidth: CGFloat { get }
  var keyEquivalentWidth: CGFloat { get }
  @discardableResult
  func stateImageRect(forBounds cellFrame: NSRect) -> NSRect
  @discardableResult
  func keyEquivalentRect(forBounds cellFrame: NSRect) -> NSRect
  func drawSeparatorItem(withFrame cellFrame: NSRect, in controlView: NSView)
  func drawStateImage(withFrame cellFrame: NSRect, in controlView: NSView)
  func drawImage(withFrame cellFrame: NSRect, in controlView: NSView)
  func drawTitle(withFrame cellFrame: NSRect, in controlView: NSView)
  func drawKeyEquivalent(withFrame cellFrame: NSRect, in controlView: NSView)
  func drawBorderAndBackground(withFrame cellFrame: NSRect, in controlView: NSView)
}
struct __micFlags {
  var needsSizing: UInt32
  var reserved: UInt32
  var needsDisplay: UInt32
  var keyEquivGlyphWidth: UInt32
  var uniqueAgainstMain: UInt32
  var RESERVED: UInt32
  init()
  init(needsSizing needsSizing: UInt32, reserved reserved: UInt32, needsDisplay needsDisplay: UInt32, keyEquivGlyphWidth keyEquivGlyphWidth: UInt32, uniqueAgainstMain uniqueAgainstMain: UInt32, RESERVED RESERVED: UInt32)
}

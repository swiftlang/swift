
class CGFont {
}
typealias CGFontIndex = UInt16
typealias CGGlyph = CGFontIndex
enum CGFontPostScriptFormat : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case type1
  case type3
  case type42
}
let kCGFontIndexMax: CGFontIndex
let kCGFontIndexInvalid: CGFontIndex
let kCGGlyphMax: CGFontIndex
extension CGFont {
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.5, *)
  init?(with provider: CGDataProvider?)
  @available(OSX 10.5, *)
  init?(withFontName name: CFString?)
  @available(OSX 10.4, *)
  init?(copyWithVariationsFont font: CGFont?, variations variations: CFDictionary?)
  @available(OSX 10.0, *)
  var numberOfGlyphs: Int { get }
  @available(OSX 10.0, *)
  var unitsPerEm: Int32 { get }
  @available(OSX 10.4, *)
  @discardableResult
  func copyPostScriptName() -> CFString?
  @available(OSX 10.5, *)
  @discardableResult
  func copyFullName() -> CFString?
  @available(OSX 10.5, *)
  var ascent: Int32 { get }
  @available(OSX 10.5, *)
  var descent: Int32 { get }
  @available(OSX 10.5, *)
  var leading: Int32 { get }
  @available(OSX 10.5, *)
  var capHeight: Int32 { get }
  @available(OSX 10.5, *)
  var xHeight: Int32 { get }
  @available(OSX 10.5, *)
  var fontBBox: CGRect { get }
  @available(OSX 10.5, *)
  var italicAngle: CGFloat { get }
  @available(OSX 10.5, *)
  var stemV: CGFloat { get }
  @available(OSX 10.4, *)
  @discardableResult
  func copyVariationAxes() -> CFArray?
  @available(OSX 10.4, *)
  @discardableResult
  func copyVariations() -> CFDictionary?
  @available(OSX 10.0, *)
  @discardableResult
  func getGlyphAdvances(glyphs glyphs: UnsafePointer<CGGlyph>, count count: Int, advances advances: UnsafeMutablePointer<Int32>) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func getGlyphBBoxes(glyphs glyphs: UnsafePointer<CGGlyph>, count count: Int, bboxes bboxes: UnsafeMutablePointer<CGRect>) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func getGlyphWithGlyphName(name name: CFString?) -> CGGlyph
  @available(OSX 10.5, *)
  @discardableResult
  func copyGlyphNameForGlyph(glyph glyph: CGGlyph) -> CFString?
  @available(OSX 10.4, *)
  @discardableResult
  func canCreatePostScriptSubset(_ format: CGFontPostScriptFormat) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  func createPostScriptSubset(subsetName subsetName: CFString?, format format: CGFontPostScriptFormat, glyphs glyphs: UnsafePointer<CGGlyph>?, count count: Int, encoding encoding: UnsafePointer<CGGlyph>!) -> CFData?
  @available(OSX 10.4, *)
  @discardableResult
  func createPostScriptEncoding(encoding encoding: UnsafePointer<CGGlyph>!) -> CFData?
  @available(OSX 10.5, *)
  @discardableResult
  func copyTableTags() -> CFArray?
  @available(OSX 10.5, *)
  @discardableResult
  func copyTableForTag(_ tag: UInt32) -> CFData?
  @available(OSX 10.4, *)
  class let variationAxisName: CFString
  @available(OSX 10.4, *)
  class let variationAxisMinValue: CFString
  @available(OSX 10.4, *)
  class let variationAxisMaxValue: CFString
  @available(OSX 10.4, *)
  class let variationAxisDefaultValue: CFString
}


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
  @available(watchOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(watchOS 2.0, *)
  init?(with provider: CGDataProvider?)
  @available(watchOS 2.0, *)
  init?(withFontName name: CFString?)
  @available(watchOS 2.0, *)
  init?(copyWithVariationsFont font: CGFont?, variations variations: CFDictionary?)
  @available(watchOS 2.0, *)
  var numberOfGlyphs: Int { get }
  @available(watchOS 2.0, *)
  var unitsPerEm: Int32 { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func copyPostScriptName() -> CFString?
  @available(watchOS 2.0, *)
  @discardableResult
  func copyFullName() -> CFString?
  @available(watchOS 2.0, *)
  var ascent: Int32 { get }
  @available(watchOS 2.0, *)
  var descent: Int32 { get }
  @available(watchOS 2.0, *)
  var leading: Int32 { get }
  @available(watchOS 2.0, *)
  var capHeight: Int32 { get }
  @available(watchOS 2.0, *)
  var xHeight: Int32 { get }
  @available(watchOS 2.0, *)
  var fontBBox: CGRect { get }
  @available(watchOS 2.0, *)
  var italicAngle: CGFloat { get }
  @available(watchOS 2.0, *)
  var stemV: CGFloat { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func copyVariationAxes() -> CFArray?
  @available(watchOS 2.0, *)
  @discardableResult
  func copyVariations() -> CFDictionary?
  @available(watchOS 2.0, *)
  @discardableResult
  func getGlyphAdvances(glyphs glyphs: UnsafePointer<CGGlyph>, count count: Int, advances advances: UnsafeMutablePointer<Int32>) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func getGlyphBBoxes(glyphs glyphs: UnsafePointer<CGGlyph>, count count: Int, bboxes bboxes: UnsafeMutablePointer<CGRect>) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func getGlyphWithGlyphName(name name: CFString?) -> CGGlyph
  @available(watchOS 2.0, *)
  @discardableResult
  func copyGlyphNameForGlyph(glyph glyph: CGGlyph) -> CFString?
  @available(watchOS 2.0, *)
  @discardableResult
  func canCreatePostScriptSubset(_ format: CGFontPostScriptFormat) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func createPostScriptSubset(subsetName subsetName: CFString?, format format: CGFontPostScriptFormat, glyphs glyphs: UnsafePointer<CGGlyph>?, count count: Int, encoding encoding: UnsafePointer<CGGlyph>!) -> CFData?
  @available(watchOS 2.0, *)
  @discardableResult
  func createPostScriptEncoding(encoding encoding: UnsafePointer<CGGlyph>!) -> CFData?
  @available(watchOS 2.0, *)
  @discardableResult
  func copyTableTags() -> CFArray?
  @available(watchOS 2.0, *)
  @discardableResult
  func copyTableForTag(_ tag: UInt32) -> CFData?
  @available(watchOS 2.0, *)
  class let variationAxisName: CFString
  @available(watchOS 2.0, *)
  class let variationAxisMinValue: CFString
  @available(watchOS 2.0, *)
  class let variationAxisMaxValue: CFString
  @available(watchOS 2.0, *)
  class let variationAxisDefaultValue: CFString
}
enum CGGlyphDeprecatedEnum : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  @available(*, deprecated)
  case min
  @available(*, deprecated)
  case max
}

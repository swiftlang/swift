
class CTGlyphInfo {
}
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoGetTypeID() -> CFTypeID
enum CTCharacterCollection : UInt16 {
  init?(rawValue rawValue: UInt16)
  var rawValue: UInt16 { get }
  @available(OSX 10.8, *)
  case identityMapping
  @available(OSX 10.8, *)
  case adobeCNS1
  @available(OSX 10.8, *)
  case adobeGB1
  @available(OSX 10.8, *)
  case adobeJapan1
  @available(OSX 10.8, *)
  case adobeJapan2
  @available(OSX 10.8, *)
  case adobeKorea1
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTIdentityMappingCharacterCollection: CTCharacterCollection { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTAdobeCNS1CharacterCollection: CTCharacterCollection { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTAdobeGB1CharacterCollection: CTCharacterCollection { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTAdobeJapan1CharacterCollection: CTCharacterCollection { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTAdobeJapan2CharacterCollection: CTCharacterCollection { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTAdobeKorea1CharacterCollection: CTCharacterCollection { get }
}
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoCreateWithGlyphName(_ glyphName: CFString, _ font: CTFont, _ baseString: CFString) -> CTGlyphInfo
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoCreateWithGlyph(_ glyph: CGGlyph, _ font: CTFont, _ baseString: CFString) -> CTGlyphInfo
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoCreateWithCharacterIdentifier(_ cid: CGFontIndex, _ collection: CTCharacterCollection, _ baseString: CFString) -> CTGlyphInfo
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoGetGlyphName(_ glyphInfo: CTGlyphInfo) -> CFString?
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoGetCharacterIdentifier(_ glyphInfo: CTGlyphInfo) -> CGFontIndex
@available(OSX 10.5, *)
@discardableResult
func CTGlyphInfoGetCharacterCollection(_ glyphInfo: CTGlyphInfo) -> CTCharacterCollection

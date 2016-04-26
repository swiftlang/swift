
class CTGlyphInfo {
}
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoGetTypeID() -> CFTypeID
enum CTCharacterCollection : UInt16 {
  init?(rawValue rawValue: UInt16)
  var rawValue: UInt16 { get }
  @available(watchOS 2.0, *)
  case identityMapping
  @available(watchOS 2.0, *)
  case adobeCNS1
  @available(watchOS 2.0, *)
  case adobeGB1
  @available(watchOS 2.0, *)
  case adobeJapan1
  @available(watchOS 2.0, *)
  case adobeJapan2
  @available(watchOS 2.0, *)
  case adobeKorea1
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTIdentityMappingCharacterCollection: CTCharacterCollection { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTAdobeCNS1CharacterCollection: CTCharacterCollection { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTAdobeGB1CharacterCollection: CTCharacterCollection { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTAdobeJapan1CharacterCollection: CTCharacterCollection { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTAdobeJapan2CharacterCollection: CTCharacterCollection { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTAdobeKorea1CharacterCollection: CTCharacterCollection { get }
}
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoCreateWithGlyphName(_ glyphName: CFString, _ font: CTFont, _ baseString: CFString) -> CTGlyphInfo
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoCreateWithGlyph(_ glyph: CGGlyph, _ font: CTFont, _ baseString: CFString) -> CTGlyphInfo
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoCreateWithCharacterIdentifier(_ cid: CGFontIndex, _ collection: CTCharacterCollection, _ baseString: CFString) -> CTGlyphInfo
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoGetGlyphName(_ glyphInfo: CTGlyphInfo) -> CFString?
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoGetCharacterIdentifier(_ glyphInfo: CTGlyphInfo) -> CGFontIndex
@available(watchOS 2.0, *)
@discardableResult
func CTGlyphInfoGetCharacterCollection(_ glyphInfo: CTGlyphInfo) -> CTCharacterCollection

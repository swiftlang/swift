
enum NSCharacterCollection : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case identityMappingCharacterCollection
  case adobeCNS1CharacterCollection
  case adobeGB1CharacterCollection
  case adobeJapan1CharacterCollection
  case adobeJapan2CharacterCollection
  case adobeKorea1CharacterCollection
}
class NSGlyphInfo : NSObject, NSCopying, NSSecureCoding {
  /*not inherited*/ init?(glyphName glyphName: String, for font: NSFont, baseString theString: String)
  /*not inherited*/ init?(glyph glyph: NSGlyph, for font: NSFont, baseString theString: String)
  /*not inherited*/ init?(characterIdentifier cid: Int, collection characterCollection: NSCharacterCollection, baseString theString: String)
  var glyphName: String? { get }
  var characterIdentifier: Int { get }
  var characterCollection: NSCharacterCollection { get }
}

// Prototype code to be added to an eventual UnicodeStorage.swift

import Swift

extension UnicodeStorage {
  func transcoded<OtherEncoding: UnicodeEncoding>(
    to otherEncoding: OtherEncoding.Type
  ) -> TranscodedView<OtherEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: otherEncoding)
  }

  var characters: CharacterView {
    return CharacterView(codeUnits, Encoding.self)
  }
}

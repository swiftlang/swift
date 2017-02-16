// Prototype code to be added to an eventual UnicodeStorage.swift

import Swift

extension UnicodeStorage {
  func transcoded<OtherEncoding: UnicodeEncoding>(
    to otherEncoding: OtherEncoding.Type
  ) -> TranscodedView<OtherEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: otherEncoding)
  }

  typealias Characters = CharacterView<CodeUnits, Encoding>
  var characters: Characters {
    return Characters(codeUnits, Encoding.self)
  }
}

import SwiftRemoteMirror

extension DefaultStringInterpolation {
  mutating func appendInterpolation(hex: swift_reflection_ptr_t) {
    appendInterpolation("0x")
    appendInterpolation(String(hex, radix: 16))
  }
}

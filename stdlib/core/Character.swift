// \brief Character represents some Unicode grapheme cluster as
// defined by a canonical, localized, or otherwise tailored
// segmentation algorithm.  
enum Character {
  // Fundamentally, it is just a String, but it is optimized for the
  // common case where the UTF-8 representation fits in 63 bits.  The
  // remaining bit is used to discriminate between small and large
  // representations.  In the small representation, the unused bytes 
  // are filled with 0xFF
  case SmallRepresentation(Builtin.Int63)
  case LargeRepresentation(OnHeap<String>)

  init(s: String) {
    var length = s.str_value.length
    alwaysTrap(length > 0, "Can't form a Character from an empty String")
    // The small representation can accept up to 8 code units as long
    // as the last one is a continuation.  Since the high bit of the
    // last byte is used for the enum's discriminator, we have to
    // reconstruct it.  As a result, we can't store 0x7f in the final
    // byte, because we wouldn't be able to distinguish it from an
    // unused 0xFF byte.  Rather than trying to squeeze in other
    // one-byte code points there, we simplify decoding by banning
    // starting a code point in the last byte, and assuming that its
    // high bit is 1.
    if length < 8 || ((length == 8) && (s.str_value[7] >= 0x80)) {
      var asInt = -1      // Start with all bits set.

      // Read the string backwards, shifting code units into the low
      // byte
      while length-- != 0 {
        asInt <<= 8
        asInt |= Int(s.str_value[length])
      }
      self = SmallRepresentation(Builtin.trunc_Int64_Int63(asInt.value))
    }
    else {
      self = LargeRepresentation(OnHeap(s))
    }
  }

  static func _smallSize(value: UInt64) -> Int {
    for (var i = 0; i < 8; ++i) {
      if (value & 0xFF) == 0xFF {
        return i
      }
      value >>= 8
    }
    return 8
  }

  static func _smallValue(value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value)) | (1<<63)
  }

  func [conversion] __conversion() -> String {
    switch self {
    case .SmallRepresentation(var _63bits):
      var value = Character._smallValue(_63bits)
      var size = Character._smallSize(value)
      var resultBytes = StringByteData(size)
      resultBytes.appendBytes(UnsafePointer(Builtin.addressof(&value)), size)
      return String(resultBytes)
    case .LargeRepresentation(var value):
      return value
    }
  }
}

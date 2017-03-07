import Swift

// FIXME: when doing the swap, this will be the real Swift 3 String
internal typealias OldString = Swift.String

// Methods on new String for Swift 3 compatibility and integration purposes.
// To be either deprecated in future, or removed after the new String gains
// them natively.

// FIXME: These methods for temporary shimming only, to be removed
extension String {
  // Some non-String code within the stdlib accesses this on the old String.
  // This is here to help integration and needs to be removed once those
  // are addressed.
  public var _core: _StringCore {
    return OldString(self)._core
  }
  
  // Create a new String from an old String
  init(_old: OldString) {
    self.init(canonical: 
      SwiftCanonicalString(codeUnits: Array(_old.utf16),
                           encodedWith: UTF16.self
    ))
  }
}


// Deprecated CString interop (does not include Foundation methods)
extension String {
  @available(*, deprecated, message: "TODO")
  public init(cString: UnsafePointer<UInt8>) {
    self.init(cString: cString, encoding: UTF8.self)
  }

  @available(*, deprecated, message: "TODO")
  // TODO: should this continue to work for the non-repairing case?
  public init?(validatingUTF8 cString: UnsafePointer<CChar>) {
    self.init(cString: cString)
  }

  @available(*, deprecated, message: "TODO")
  // TODO: should this continue to work for the repairing=false case?
  public static func decodeCString<Encoding : UnicodeCodec>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true)
      -> (result: String, repairsMade: Bool)? {

      return OldString.decodeCString(cString, as: encoding, 
                          repairingInvalidCodeUnits: isRepairing)
        .map { s, r in (String(_old: s),r) }
  }
  
  @available(*, deprecated, message: "TODO")
  var utf8CString: ContiguousArray<CChar> {
    var cstring = ContiguousArray<CChar>()
    cstring.reserveCapacity(utf8.count + 1)
    cstring += utf8.lazy.map(CChar.init(_:))
    cstring.append(0)
    return cstring
  }
}


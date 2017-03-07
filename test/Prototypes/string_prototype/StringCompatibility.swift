import Swift

// Methods on new String for Swift 3 compatibility and integration purposes.
// To be either deprecated in future, or removed after the new String gains
// them natively.

extension String {
  // Some non-String code within the stdlib accesses this on the old String.
  // This is here to help integration and needs to be removed once those
  // are addressed.
  public var _core: _StringCore {
    return Swift.String(self)._core
  }
}

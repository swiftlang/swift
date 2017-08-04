import Foundation

@inline(__always) @_transparent public func compareToSelf<T: Equatable>(_ t: T) -> Bool {
  return t == t
}

@inline(__always) @_transparent public func compareImportedEnumToSelf(_ e: NSRuncingMode) -> Bool {
  return compareToSelf(e)
}


@available(tvOS 3.2, *)
let kCTFontManagerErrorDomain: CFString
@available(tvOS 3.2, *)
let kCTFontManagerErrorFontURLsKey: CFString
enum CTFontManagerError : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case fileNotFound
  case insufficientPermissions
  case unrecognizedFormat
  case invalidFontData
  case alreadyRegistered
  case notRegistered
  case inUse
  case systemRequired
}

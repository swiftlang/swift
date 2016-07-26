
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerCreateFontDescriptorsFromURL(_ fileURL: CFURL) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerCreateFontDescriptorFromData(_ data: CFData) -> CTFontDescriptor?
enum CTFontManagerScope : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case none
  case process
  case user
  case session
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerRegisterFontsForURL(_ fontURL: CFURL, _ scope: CTFontManagerScope, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerUnregisterFontsForURL(_ fontURL: CFURL, _ scope: CTFontManagerScope, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerRegisterGraphicsFont(_ font: CGFont, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerUnregisterGraphicsFont(_ font: CGFont, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerRegisterFontsForURLs(_ fontURLs: CFArray, _ scope: CTFontManagerScope, _ errors: UnsafeMutablePointer<Unmanaged<CFArray>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontManagerUnregisterFontsForURLs(_ fontURLs: CFArray, _ scope: CTFontManagerScope, _ errors: UnsafeMutablePointer<Unmanaged<CFArray>?>?) -> Bool
enum CTFontManagerAutoActivationSetting : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case `default`
  case disabled
  case enabled
  case promptUser
}
@available(watchOS 2.0, *)
let kCTFontManagerRegisteredFontsChangedNotification: CFString

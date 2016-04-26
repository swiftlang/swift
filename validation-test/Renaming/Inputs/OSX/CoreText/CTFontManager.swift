
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCopyAvailablePostScriptNames() -> CFArray
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCopyAvailableFontFamilyNames() -> CFArray
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCopyAvailableFontURLs() -> CFArray
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCompareFontFamilyNames(_ family1: UnsafePointer<Void>, _ family2: UnsafePointer<Void>, _ context: UnsafeMutablePointer<Void>?) -> CFComparisonResult
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCreateFontDescriptorsFromURL(_ fileURL: CFURL) -> CFArray?
@available(OSX 10.7, *)
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
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerRegisterFontsForURL(_ fontURL: CFURL, _ scope: CTFontManagerScope, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerUnregisterFontsForURL(_ fontURL: CFURL, _ scope: CTFontManagerScope, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CTFontManagerRegisterGraphicsFont(_ font: CGFont, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CTFontManagerUnregisterGraphicsFont(_ font: CGFont, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerRegisterFontsForURLs(_ fontURLs: CFArray, _ scope: CTFontManagerScope, _ errors: UnsafeMutablePointer<Unmanaged<CFArray>?>?) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerUnregisterFontsForURLs(_ fontURLs: CFArray, _ scope: CTFontManagerScope, _ errors: UnsafeMutablePointer<Unmanaged<CFArray>?>?) -> Bool
@available(OSX 10.6, *)
func CTFontManagerEnableFontDescriptors(_ descriptors: CFArray, _ enable: Bool)
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerGetScopeForURL(_ fontURL: CFURL) -> CTFontManagerScope
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerIsSupportedFont(_ fontURL: CFURL) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerCreateFontRequestRunLoopSource(_ sourceOrder: CFIndex, _ createMatchesCallback: (CFDictionary, pid_t) -> Unmanaged<CFArray>) -> CFRunLoopSource?
@available(OSX 10.6, *)
let kCTFontManagerBundleIdentifier: CFString
enum CTFontManagerAutoActivationSetting : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case `default`
  case disabled
  case enabled
  case promptUser
}
@available(OSX 10.6, *)
func CTFontManagerSetAutoActivationSetting(_ bundleIdentifier: CFString?, _ setting: CTFontManagerAutoActivationSetting)
@available(OSX 10.6, *)
@discardableResult
func CTFontManagerGetAutoActivationSetting(_ bundleIdentifier: CFString?) -> CTFontManagerAutoActivationSetting
@available(OSX 10.6, *)
let kCTFontManagerRegisteredFontsChangedNotification: CFString

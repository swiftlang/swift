
@available(OSX 10.9, *)
class NSAppearance : NSObject, NSCoding {
  @available(OSX 10.9, *)
  var name: String { get }
  @discardableResult
  class func current() -> NSAppearance
  class func setCurrentAppearance(_ appearance: NSAppearance?)
  /*not inherited*/ init?(named name: String)
  init?(appearanceNamed name: String, bundle bundle: NSBundle?)
  @available(OSX 10.10, *)
  var allowsVibrancy: Bool { get }
}
@available(OSX 10.9, *)
let NSAppearanceNameAqua: String
@available(OSX, introduced: 10.9, deprecated: 10.10, message: "Light content should use the default Aqua apppearance.")
let NSAppearanceNameLightContent: String
@available(OSX 10.10, *)
let NSAppearanceNameVibrantDark: String
@available(OSX 10.10, *)
let NSAppearanceNameVibrantLight: String
protocol NSAppearanceCustomization : NSObjectProtocol {
  @available(OSX 10.9, *)
  var appearance: NSAppearance? { get set }
  @available(OSX 10.9, *)
  var effectiveAppearance: NSAppearance { get }
}

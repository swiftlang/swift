
enum NSColorSpaceModel : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case NSUnknownColorSpaceModel
  case NSGrayColorSpaceModel
  case NSRGBColorSpaceModel
  case NSCMYKColorSpaceModel
  case NSLABColorSpaceModel
  case NSDeviceNColorSpaceModel
  case NSIndexedColorSpaceModel
  case NSPatternColorSpaceModel
}
class NSColorSpace : NSObject, NSSecureCoding {
  init?(iccProfileData iccData: NSData)
  var iccProfileData: NSData? { get }
  init?(colorSyncProfile prof: UnsafeMutablePointer<Void>)
  var colorSyncProfile: UnsafeMutablePointer<Void>? { get }
  @available(OSX 10.5, *)
  init?(cgColorSpace cgColorSpace: CGColorSpace)
  @available(OSX 10.5, *)
  var cgColorSpace: CGColorSpace? { get }
  var numberOfColorComponents: Int { get }
  var colorSpaceModel: NSColorSpaceModel { get }
  var localizedName: String? { get }
  @discardableResult
  class func genericRGB() -> NSColorSpace
  @discardableResult
  class func genericGray() -> NSColorSpace
  @discardableResult
  class func genericCMYK() -> NSColorSpace
  @discardableResult
  class func deviceRGB() -> NSColorSpace
  @discardableResult
  class func deviceGray() -> NSColorSpace
  @discardableResult
  class func deviceCMYK() -> NSColorSpace
  @available(OSX 10.5, *)
  @discardableResult
  class func sRGB() -> NSColorSpace
  @available(OSX 10.6, *)
  @discardableResult
  class func genericGamma22Gray() -> NSColorSpace
  @available(OSX 10.5, *)
  @discardableResult
  class func adobeRGB1998() -> NSColorSpace
  @available(OSX 10.6, *)
  @discardableResult
  class func availableColorSpaces(with model: NSColorSpaceModel) -> [NSColorSpace]
}


class CGColorSpace {
}
enum CGColorRenderingIntent : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case defaultIntent
  case absoluteColorimetric
  case relativeColorimetric
  case perceptual
  case saturation
}
enum CGColorSpaceModel : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case unknown
  case monochrome
  case RGB
  case CMYK
  case lab
  case deviceN
  case indexed
  case pattern
}
extension CGColorSpace {
  @available(OSX 10.4, *)
  class let genericCMYK: CFString
  @available(OSX 10.10, *)
  class let displayP3: CFString
  @available(OSX 10.5, *)
  class let genericRGBLinear: CFString
  @available(OSX 10.5, *)
  class let adobeRGB1998: CFString
  @available(OSX 10.5, *)
  class let srgb: CFString
  @available(OSX 10.6, *)
  class let genericGrayGamma2_2: CFString
  @available(OSX 10.11, *)
  class let genericXYZ: CFString
  @available(OSX 10.11, *)
  class let acescgLinear: CFString
  @available(OSX 10.11, *)
  class let itur_709: CFString
  @available(OSX 10.11, *)
  class let itur_2020: CFString
  @available(OSX 10.11, *)
  class let rommrgb: CFString
  @available(OSX 10.11, *)
  class let dcip3: CFString
  @available(OSX 10.0, *)
  init?(calibratedGrayWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, gamma gamma: CGFloat)
  @available(OSX 10.0, *)
  init?(calibratedRGBWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, gamma gamma: UnsafePointer<CGFloat>!, matrix matrix: UnsafePointer<CGFloat>!)
  @available(OSX 10.0, *)
  init?(labWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, range range: UnsafePointer<CGFloat>!)
  @available(OSX 10.5, *)
  init?(withICCProfileData data: CFData?)
  @available(OSX 10.0, *)
  init?(iccBasedNComponents nComponents: Int, range range: UnsafePointer<CGFloat>?, profile profile: CGDataProvider?, alternate alternate: CGColorSpace?)
  @available(OSX 10.0, *)
  init?(indexedBaseSpace baseSpace: CGColorSpace?, last lastIndex: Int, colorTable colorTable: UnsafePointer<UInt8>?)
  @available(OSX 10.0, *)
  init?(patternBaseSpace baseSpace: CGColorSpace?)
  @available(OSX 10.0, *)
  init?(withPlatformColorSpaceRef ref: UnsafePointer<Void>?)
  @available(OSX 10.2, *)
  init?(withName name: CFString?)
  @available(OSX 10.6, *)
  @discardableResult
  func copyName() -> CFString?
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.0, *)
  var numberOfComponents: Int { get }
  @available(OSX 10.5, *)
  var model: CGColorSpaceModel { get }
  @available(OSX 10.5, *)
  var baseColorSpace: CGColorSpace? { get }
  @available(OSX 10.5, *)
  var colorTableCount: Int { get }
  @available(OSX 10.5, *)
  func getColorTable(table table: UnsafeMutablePointer<UInt8>?)
  @available(OSX 10.5, *)
  @discardableResult
  func copyICCData() -> CFData?
}
@available(OSX 10.0, *)
@discardableResult
func CGColorSpaceCreateDeviceGray() -> CGColorSpace?
@available(OSX 10.0, *)
@discardableResult
func CGColorSpaceCreateDeviceRGB() -> CGColorSpace?
@available(OSX 10.0, *)
@discardableResult
func CGColorSpaceCreateDeviceCMYK() -> CGColorSpace?

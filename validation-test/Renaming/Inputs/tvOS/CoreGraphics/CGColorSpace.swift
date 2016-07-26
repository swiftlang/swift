
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
  @available(tvOS 9.0, *)
  class let genericCMYK: CFString
  @available(tvOS 9.0, *)
  class let displayP3: CFString
  @available(tvOS 9.0, *)
  class let genericRGBLinear: CFString
  @available(tvOS 9.0, *)
  class let adobeRGB1998: CFString
  @available(tvOS 9.0, *)
  class let srgb: CFString
  @available(tvOS 9.0, *)
  class let genericGrayGamma2_2: CFString
  @available(tvOS 9.0, *)
  class let genericXYZ: CFString
  @available(tvOS 9.0, *)
  class let acescgLinear: CFString
  @available(tvOS 9.0, *)
  class let itur_709: CFString
  @available(tvOS 9.0, *)
  class let itur_2020: CFString
  @available(tvOS 9.0, *)
  class let rommrgb: CFString
  @available(tvOS 9.0, *)
  class let dcip3: CFString
  @available(tvOS 2.0, *)
  init?(calibratedGrayWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, gamma gamma: CGFloat)
  @available(tvOS 2.0, *)
  init?(calibratedRGBWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, gamma gamma: UnsafePointer<CGFloat>!, matrix matrix: UnsafePointer<CGFloat>!)
  @available(tvOS 2.0, *)
  init?(labWhitePoint whitePoint: UnsafePointer<CGFloat>!, blackPoint blackPoint: UnsafePointer<CGFloat>!, range range: UnsafePointer<CGFloat>!)
  @available(tvOS 2.0, *)
  init?(withICCProfileData data: CFData?)
  @available(tvOS 2.0, *)
  init?(iccBasedNComponents nComponents: Int, range range: UnsafePointer<CGFloat>?, profile profile: CGDataProvider?, alternate alternate: CGColorSpace?)
  @available(tvOS 2.0, *)
  init?(indexedBaseSpace baseSpace: CGColorSpace?, last lastIndex: Int, colorTable colorTable: UnsafePointer<UInt8>?)
  @available(tvOS 2.0, *)
  init?(patternBaseSpace baseSpace: CGColorSpace?)
  @available(tvOS 9.0, *)
  init?(withPlatformColorSpaceRef ref: UnsafePointer<Void>?)
  @available(tvOS 2.0, *)
  init?(withName name: CFString?)
  @available(tvOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(tvOS 2.0, *)
  var numberOfComponents: Int { get }
  @available(tvOS 2.0, *)
  var model: CGColorSpaceModel { get }
  @available(tvOS 2.0, *)
  var baseColorSpace: CGColorSpace? { get }
  @available(tvOS 2.0, *)
  var colorTableCount: Int { get }
  @available(tvOS 2.0, *)
  func getColorTable(table table: UnsafeMutablePointer<UInt8>?)
  @available(tvOS 6.0, *)
  @discardableResult
  func copyICCData() -> CFData?
}
@available(tvOS 2.0, *)
@discardableResult
func CGColorSpaceCreateDeviceGray() -> CGColorSpace?
@available(tvOS 2.0, *)
@discardableResult
func CGColorSpaceCreateDeviceRGB() -> CGColorSpace?
@available(tvOS 2.0, *)
@discardableResult
func CGColorSpaceCreateDeviceCMYK() -> CGColorSpace?

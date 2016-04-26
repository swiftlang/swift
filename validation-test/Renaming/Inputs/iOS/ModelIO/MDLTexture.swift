
enum MDLTextureChannelEncoding : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case uInt8
  static var uint8: MDLTextureChannelEncoding { get }
  case uInt16
  static var uint16: MDLTextureChannelEncoding { get }
  case uInt24
  static var uint24: MDLTextureChannelEncoding { get }
  case uInt32
  static var uint32: MDLTextureChannelEncoding { get }
  case float16
  case float32
}
@available(iOS 9.0, *)
class MDLTexture : NSObject, MDLNamed {
  convenience init?(named name: String)
  convenience init?(named name: String, bundle bundleOrNil: NSBundle?)
  convenience init?(cubeWithImagesNamed names: [String])
  convenience init?(cubeWithImagesNamed names: [String], bundle bundleOrNil: NSBundle?)
  @discardableResult
  class func irradianceTextureCube(with texture: MDLTexture, name name: String?, dimensions dimensions: vector_int2) -> Self
  @discardableResult
  class func irradianceTextureCube(with texture: MDLTexture, name name: String?, dimensions dimensions: vector_int2, roughness roughness: Float) -> Self
  init(data pixelData: NSData?, topLeftOrigin topLeftOrigin: Bool, name name: String?, dimensions dimensions: vector_int2, rowStride rowStride: Int, channelCount channelCount: Int, channelEncoding channelEncoding: MDLTextureChannelEncoding, isCube isCube: Bool)
  @discardableResult
  func write(to URL: NSURL) -> Bool
  @discardableResult
  func write(to nsurl: NSURL, type type: CFString) -> Bool
  @discardableResult
  func imageFromTexture() -> Unmanaged<CGImage>?
  @discardableResult
  func texelDataWithTopLeftOrigin() -> NSData?
  @discardableResult
  func texelDataWithBottomLeftOrigin() -> NSData?
  @discardableResult
  func texelDataWithTopLeftOrigin(atMipLevel level: Int, create create: Bool) -> NSData?
  @discardableResult
  func texelDataWithBottomLeftOrigin(atMipLevel level: Int, create create: Bool) -> NSData?
  var dimensions: vector_int2 { get }
  var rowStride: Int { get }
  var channelCount: Int { get }
  var mipLevelCount: Int { get }
  var channelEncoding: MDLTextureChannelEncoding { get }
  var isCube: Bool
}
@available(iOS 9.0, *)
class MDLURLTexture : MDLTexture {
  init(url URL: NSURL, name name: String?)
  @NSCopying var url: NSURL
}
@available(iOS 9.0, *)
class MDLCheckerboardTexture : MDLTexture {
  init(divisions divisions: Float, name name: String?, dimensions dimensions: vector_int2, channelCount channelCount: Int32, channelEncoding channelEncoding: MDLTextureChannelEncoding, color1 color1: CGColor, color2 color2: CGColor)
  var divisions: Float
  var color1: CGColor?
  var color2: CGColor?
}
@available(iOS 9.0, *)
class MDLSkyCubeTexture : MDLTexture {
  init(name name: String?, channelEncoding channelEncoding: MDLTextureChannelEncoding, textureDimensions textureDimensions: vector_int2, turbidity turbidity: Float, sunElevation sunElevation: Float, upperAtmosphereScattering upperAtmosphereScattering: Float, groundAlbedo groundAlbedo: Float)
  func update()
  var turbidity: Float
  var sunElevation: Float
  var upperAtmosphereScattering: Float
  var groundAlbedo: Float
  var horizonElevation: Float
  var groundColor: CGColor?
  var gamma: Float
  var exposure: Float
  var brightness: Float
  var contrast: Float
  var saturation: Float
  var highDynamicRangeCompression: vector_float2
}
@available(iOS 9.0, *)
class MDLColorSwatchTexture : MDLTexture {
  init(colorTemperatureGradientFrom colorTemperature1: Float, toColorTemperature colorTemperature2: Float, name name: String?, textureDimensions textureDimensions: vector_int2)
  init(colorGradientFrom color1: CGColor, to color2: CGColor, name name: String?, textureDimensions textureDimensions: vector_int2)
}
@available(iOS 9.0, *)
class MDLNoiseTexture : MDLTexture {
  init(vectorNoiseWithSmoothness smoothness: Float, name name: String?, textureDimensions textureDimensions: vector_int2, channelEncoding channelEncoding: MDLTextureChannelEncoding)
  init(scalarNoiseWithSmoothness smoothness: Float, name name: String?, textureDimensions textureDimensions: vector_int2, channelCount channelCount: Int32, channelEncoding channelEncoding: MDLTextureChannelEncoding, grayscale grayscale: Bool)
}
@available(iOS 9.0, *)
class MDLNormalMapTexture : MDLTexture {
  init(byGeneratingNormalMapWith sourceTexture: MDLTexture, name name: String?, smoothness smoothness: Float, contrast contrast: Float)
}

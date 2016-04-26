
enum NSTIFFCompression : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case CCITTFAX3
  case CCITTFAX4
  case LZW
  case JPEG
  case NEXT
  case packBits
  case oldJPEG
}
enum NSBitmapImageFileType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case NSTIFFFileType
  case NSBMPFileType
  case NSGIFFileType
  case NSJPEGFileType
  case NSPNGFileType
  case NSJPEG2000FileType
}
enum NSImageRepLoadStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknownType
  case readingHeader
  case willNeedAllData
  case invalidData
  case unexpectedEOF
  case completed
}
struct NSBitmapFormat : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var NSAlphaFirstBitmapFormat: NSBitmapFormat { get }
  static var NSAlphaNonpremultipliedBitmapFormat: NSBitmapFormat { get }
  static var NSFloatingPointSamplesBitmapFormat: NSBitmapFormat { get }
  @available(OSX 10.10, *)
  static var NS16BitLittleEndianBitmapFormat: NSBitmapFormat { get }
  @available(OSX 10.10, *)
  static var NS32BitLittleEndianBitmapFormat: NSBitmapFormat { get }
  @available(OSX 10.10, *)
  static var NS16BitBigEndianBitmapFormat: NSBitmapFormat { get }
  @available(OSX 10.10, *)
  static var NS32BitBigEndianBitmapFormat: NSBitmapFormat { get }
}
let NSImageCompressionMethod: String
let NSImageCompressionFactor: String
let NSImageDitherTransparency: String
let NSImageRGBColorTable: String
let NSImageInterlaced: String
let NSImageColorSyncProfileData: String
let NSImageFrameCount: String
let NSImageCurrentFrame: String
let NSImageCurrentFrameDuration: String
let NSImageLoopCount: String
let NSImageGamma: String
let NSImageProgressive: String
let NSImageEXIFData: String
@available(OSX 10.5, *)
let NSImageFallbackBackgroundColor: String
class NSBitmapImageRep : NSImageRep, NSSecureCoding {
  init?(focusedViewRect rect: NSRect)
  init?(bitmapDataPlanes planes: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>?, pixelsWide width: Int, pixelsHigh height: Int, bitsPerSample bps: Int, samplesPerPixel spp: Int, hasAlpha alpha: Bool, isPlanar isPlanar: Bool, colorSpaceName colorSpaceName: String, bytesPerRow rBytes: Int, bitsPerPixel pBits: Int)
  init?(bitmapDataPlanes planes: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>?, pixelsWide width: Int, pixelsHigh height: Int, bitsPerSample bps: Int, samplesPerPixel spp: Int, hasAlpha alpha: Bool, isPlanar isPlanar: Bool, colorSpaceName colorSpaceName: String, bitmapFormat bitmapFormat: NSBitmapFormat, bytesPerRow rBytes: Int, bitsPerPixel pBits: Int)
  @available(OSX 10.5, *)
  init(cgImage cgImage: CGImage)
  @available(OSX 10.5, *)
  init(ciImage ciImage: CIImage)
  @discardableResult
  class func imageReps(with data: NSData) -> [NSImageRep]
  init?(data data: NSData)
  var bitmapData: UnsafeMutablePointer<UInt8>? { get }
  func getBitmapDataPlanes(_ data: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>)
  var isPlanar: Bool { get }
  var samplesPerPixel: Int { get }
  var bitsPerPixel: Int { get }
  var bytesPerRow: Int { get }
  var bytesPerPlane: Int { get }
  var numberOfPlanes: Int { get }
  var bitmapFormat: NSBitmapFormat { get }
  func getCompression(_ compression: UnsafeMutablePointer<NSTIFFCompression>?, factor factor: UnsafeMutablePointer<Float>?)
  func setCompression(_ compression: NSTIFFCompression, factor factor: Float)
  @NSCopying var tiffRepresentation: NSData? { get }
  @discardableResult
  func tiffRepresentation(using comp: NSTIFFCompression, factor factor: Float) -> NSData?
  @discardableResult
  class func tiffRepresentationOfImageReps(in array: [NSImageRep]) -> NSData?
  @discardableResult
  class func tiffRepresentationOfImageReps(in array: [NSImageRep], using comp: NSTIFFCompression, factor factor: Float) -> NSData?
  class func getTIFFCompressionTypes(_ list: UnsafeMutablePointer<UnsafePointer<NSTIFFCompression>?>, count numTypes: UnsafeMutablePointer<Int>)
  @discardableResult
  class func localizedName(forTIFFCompressionType compression: NSTIFFCompression) -> String?
  @discardableResult
  func canBeCompressed(using compression: NSTIFFCompression) -> Bool
  func colorize(byMappingGray midPoint: CGFloat, to midPointColor: NSColor?, blackMapping shadowColor: NSColor?, whiteMapping lightColor: NSColor?)
  init(forIncrementalLoad forIncrementalLoad: ())
  @discardableResult
  func incrementalLoad(from data: NSData, complete complete: Bool) -> Int
  func setColor(_ color: NSColor, atX x: Int, y y: Int)
  @discardableResult
  func colorAt(x x: Int, y y: Int) -> NSColor?
  func getPixel(_ p: UnsafeMutablePointer<Int>!, atX x: Int, y y: Int)
  func setPixel(_ p: UnsafeMutablePointer<Int>!, atX x: Int, y y: Int)
  @available(OSX 10.5, *)
  var cgImage: CGImage? { get }
  @available(OSX 10.6, *)
  var colorSpace: NSColorSpace { get }
  @available(OSX 10.6, *)
  @discardableResult
  func converting(to targetSpace: NSColorSpace, renderingIntent renderingIntent: NSColorRenderingIntent) -> NSBitmapImageRep?
  @available(OSX 10.6, *)
  @discardableResult
  func retagging(with newSpace: NSColorSpace) -> NSBitmapImageRep?
}
struct __bitmapRepFlags {
  var bitsPerPixel: UInt32
  var isPlanar: UInt32
  var explicitPlanes: UInt32
  var imageSourceIsIndexed: UInt32
  var dataLoaded: UInt32
  var colorModel: UInt32
  var tierTwoInfoIsLoaded: UInt32
  var respectO: UInt32
  var compressionFactor: UInt32
  var imageNumber: UInt32
  var bitmapFormat: UInt32
  var cgImageIsPrimary: UInt32
  var compression: UInt32
  init()
  init(bitsPerPixel bitsPerPixel: UInt32, isPlanar isPlanar: UInt32, explicitPlanes explicitPlanes: UInt32, imageSourceIsIndexed imageSourceIsIndexed: UInt32, dataLoaded dataLoaded: UInt32, colorModel colorModel: UInt32, tierTwoInfoIsLoaded tierTwoInfoIsLoaded: UInt32, respectO respectO: UInt32, compressionFactor compressionFactor: UInt32, imageNumber imageNumber: UInt32, bitmapFormat bitmapFormat: UInt32, cgImageIsPrimary cgImageIsPrimary: UInt32, compression compression: UInt32)
}
extension NSBitmapImageRep {
  @discardableResult
  class func representationOfImageReps(in imageReps: [NSImageRep], using storageType: NSBitmapImageFileType, properties properties: [String : AnyObject]) -> NSData?
  @discardableResult
  func representation(using storageType: NSBitmapImageFileType, properties properties: [String : AnyObject]) -> NSData?
  func setProperty(_ property: String, withValue value: AnyObject?)
  @discardableResult
  func value(forProperty property: String) -> AnyObject?
}

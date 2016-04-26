
class CGImageSource {
}
enum CGImageSourceStatus : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case statusUnexpectedEOF
  case statusInvalidData
  case statusUnknownType
  case statusReadingHeader
  case statusIncomplete
  case statusComplete
}
@available(tvOS 4.0, *)
let kCGImageSourceTypeIdentifierHint: CFString
@available(tvOS 4.0, *)
let kCGImageSourceShouldCache: CFString
@available(tvOS 7.0, *)
let kCGImageSourceShouldCacheImmediately: CFString
@available(tvOS 4.0, *)
let kCGImageSourceShouldAllowFloat: CFString
@available(tvOS 4.0, *)
let kCGImageSourceCreateThumbnailFromImageIfAbsent: CFString
@available(tvOS 4.0, *)
let kCGImageSourceCreateThumbnailFromImageAlways: CFString
@available(tvOS 4.0, *)
let kCGImageSourceThumbnailMaxPixelSize: CFString
@available(tvOS 4.0, *)
let kCGImageSourceCreateThumbnailWithTransform: CFString
@available(tvOS 9.0, *)
let kCGImageSourceSubsampleFactor: CFString
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceGetTypeID() -> CFTypeID
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCopyTypeIdentifiers() -> CFArray
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateWithDataProvider(_ provider: CGDataProvider, _ options: CFDictionary?) -> CGImageSource?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateWithData(_ data: CFData, _ options: CFDictionary?) -> CGImageSource?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateWithURL(_ url: CFURL, _ options: CFDictionary?) -> CGImageSource?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceGetType(_ isrc: CGImageSource) -> CFString?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceGetCount(_ isrc: CGImageSource) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCopyProperties(_ isrc: CGImageSource, _ options: CFDictionary?) -> CFDictionary?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCopyPropertiesAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CFDictionary?
@available(tvOS 7.0, *)
@discardableResult
func CGImageSourceCopyMetadataAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImageMetadata?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateImageAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImage?
@available(tvOS 7.0, *)
func CGImageSourceRemoveCacheAtIndex(_ isrc: CGImageSource, _ index: Int)
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateThumbnailAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImage?
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceCreateIncremental(_ options: CFDictionary?) -> CGImageSource
@available(tvOS 4.0, *)
func CGImageSourceUpdateData(_ isrc: CGImageSource, _ data: CFData, _ final: Bool)
@available(tvOS 4.0, *)
func CGImageSourceUpdateDataProvider(_ isrc: CGImageSource, _ provider: CGDataProvider, _ final: Bool)
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceGetStatus(_ isrc: CGImageSource) -> CGImageSourceStatus
@available(tvOS 4.0, *)
@discardableResult
func CGImageSourceGetStatusAtIndex(_ isrc: CGImageSource, _ index: Int) -> CGImageSourceStatus

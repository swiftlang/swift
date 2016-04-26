
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
@available(OSX 10.4, *)
let kCGImageSourceTypeIdentifierHint: CFString
@available(OSX 10.4, *)
let kCGImageSourceShouldCache: CFString
@available(OSX 10.9, *)
let kCGImageSourceShouldCacheImmediately: CFString
@available(OSX 10.4, *)
let kCGImageSourceShouldAllowFloat: CFString
@available(OSX 10.4, *)
let kCGImageSourceCreateThumbnailFromImageIfAbsent: CFString
@available(OSX 10.4, *)
let kCGImageSourceCreateThumbnailFromImageAlways: CFString
@available(OSX 10.4, *)
let kCGImageSourceThumbnailMaxPixelSize: CFString
@available(OSX 10.4, *)
let kCGImageSourceCreateThumbnailWithTransform: CFString
@available(OSX 10.11, *)
let kCGImageSourceSubsampleFactor: CFString
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCopyTypeIdentifiers() -> CFArray
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateWithDataProvider(_ provider: CGDataProvider, _ options: CFDictionary?) -> CGImageSource?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateWithData(_ data: CFData, _ options: CFDictionary?) -> CGImageSource?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateWithURL(_ url: CFURL, _ options: CFDictionary?) -> CGImageSource?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceGetType(_ isrc: CGImageSource) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceGetCount(_ isrc: CGImageSource) -> Int
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCopyProperties(_ isrc: CGImageSource, _ options: CFDictionary?) -> CFDictionary?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCopyPropertiesAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CFDictionary?
@available(OSX 10.8, *)
@discardableResult
func CGImageSourceCopyMetadataAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImageMetadata?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateImageAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImage?
@available(OSX 10.9, *)
func CGImageSourceRemoveCacheAtIndex(_ isrc: CGImageSource, _ index: Int)
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateThumbnailAtIndex(_ isrc: CGImageSource, _ index: Int, _ options: CFDictionary?) -> CGImage?
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceCreateIncremental(_ options: CFDictionary?) -> CGImageSource
@available(OSX 10.4, *)
func CGImageSourceUpdateData(_ isrc: CGImageSource, _ data: CFData, _ final: Bool)
@available(OSX 10.4, *)
func CGImageSourceUpdateDataProvider(_ isrc: CGImageSource, _ provider: CGDataProvider, _ final: Bool)
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceGetStatus(_ isrc: CGImageSource) -> CGImageSourceStatus
@available(OSX 10.4, *)
@discardableResult
func CGImageSourceGetStatusAtIndex(_ isrc: CGImageSource, _ index: Int) -> CGImageSourceStatus

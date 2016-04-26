
class CGImageDestination {
}
@available(OSX 10.4, *)
let kCGImageDestinationLossyCompressionQuality: CFString
@available(OSX 10.4, *)
let kCGImageDestinationBackgroundColor: CFString
@available(OSX 10.10, *)
let kCGImageDestinationImageMaxPixelSize: CFString
@available(OSX 10.10, *)
let kCGImageDestinationEmbedThumbnail: CFString
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationCopyTypeIdentifiers() -> CFArray
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationCreateWithDataConsumer(_ consumer: CGDataConsumer, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationCreateWithData(_ data: CFMutableData, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationCreateWithURL(_ url: CFURL, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(OSX 10.4, *)
func CGImageDestinationSetProperties(_ idst: CGImageDestination, _ properties: CFDictionary?)
@available(OSX 10.4, *)
func CGImageDestinationAddImage(_ idst: CGImageDestination, _ image: CGImage, _ properties: CFDictionary?)
@available(OSX 10.4, *)
func CGImageDestinationAddImageFromSource(_ idst: CGImageDestination, _ isrc: CGImageSource, _ index: Int, _ properties: CFDictionary?)
@available(OSX 10.4, *)
@discardableResult
func CGImageDestinationFinalize(_ idst: CGImageDestination) -> Bool
@available(OSX 10.8, *)
func CGImageDestinationAddImageAndMetadata(_ idst: CGImageDestination, _ image: CGImage, _ metadata: CGImageMetadata?, _ options: CFDictionary?)
@available(OSX 10.8, *)
let kCGImageDestinationMetadata: CFString
@available(OSX 10.8, *)
let kCGImageDestinationMergeMetadata: CFString
@available(OSX 10.8, *)
let kCGImageMetadataShouldExcludeXMP: CFString
@available(OSX 10.10, *)
let kCGImageMetadataShouldExcludeGPS: CFString
@available(OSX 10.8, *)
let kCGImageDestinationDateTime: CFString
@available(OSX 10.8, *)
let kCGImageDestinationOrientation: CFString
@available(OSX 10.8, *)
@discardableResult
func CGImageDestinationCopyImageSource(_ idst: CGImageDestination, _ isrc: CGImageSource, _ options: CFDictionary?, _ err: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool

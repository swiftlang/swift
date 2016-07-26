
class CGImageDestination {
}
@available(iOS 4.0, *)
let kCGImageDestinationLossyCompressionQuality: CFString
@available(iOS 4.0, *)
let kCGImageDestinationBackgroundColor: CFString
@available(iOS 8.0, *)
let kCGImageDestinationImageMaxPixelSize: CFString
@available(iOS 8.0, *)
let kCGImageDestinationEmbedThumbnail: CFString
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationGetTypeID() -> CFTypeID
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationCopyTypeIdentifiers() -> CFArray
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationCreateWithDataConsumer(_ consumer: CGDataConsumer, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationCreateWithData(_ data: CFMutableData, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationCreateWithURL(_ url: CFURL, _ type: CFString, _ count: Int, _ options: CFDictionary?) -> CGImageDestination?
@available(iOS 4.0, *)
func CGImageDestinationSetProperties(_ idst: CGImageDestination, _ properties: CFDictionary?)
@available(iOS 4.0, *)
func CGImageDestinationAddImage(_ idst: CGImageDestination, _ image: CGImage, _ properties: CFDictionary?)
@available(iOS 4.0, *)
func CGImageDestinationAddImageFromSource(_ idst: CGImageDestination, _ isrc: CGImageSource, _ index: Int, _ properties: CFDictionary?)
@available(iOS 4.0, *)
@discardableResult
func CGImageDestinationFinalize(_ idst: CGImageDestination) -> Bool
@available(iOS 7.0, *)
func CGImageDestinationAddImageAndMetadata(_ idst: CGImageDestination, _ image: CGImage, _ metadata: CGImageMetadata?, _ options: CFDictionary?)
@available(iOS 7.0, *)
let kCGImageDestinationMetadata: CFString
@available(iOS 7.0, *)
let kCGImageDestinationMergeMetadata: CFString
@available(iOS 7.0, *)
let kCGImageMetadataShouldExcludeXMP: CFString
@available(iOS 8.0, *)
let kCGImageMetadataShouldExcludeGPS: CFString
@available(iOS 7.0, *)
let kCGImageDestinationDateTime: CFString
@available(iOS 7.0, *)
let kCGImageDestinationOrientation: CFString
@available(iOS 7.0, *)
@discardableResult
func CGImageDestinationCopyImageSource(_ idst: CGImageDestination, _ isrc: CGImageSource, _ options: CFDictionary?, _ err: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool

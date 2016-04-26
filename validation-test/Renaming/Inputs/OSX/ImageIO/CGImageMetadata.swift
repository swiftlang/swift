
class CGImageMetadata {
}
@discardableResult
func CGImageMetadataGetTypeID() -> CFTypeID
class CGMutableImageMetadata {
}
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCreateMutable() -> CGMutableImageMetadata
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCreateMutableCopy(_ metadata: CGImageMetadata) -> CGMutableImageMetadata?
class CGImageMetadataTag {
}
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagGetTypeID() -> CFTypeID
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceExif: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceExifAux: CFString
@available(OSX 10.9, *)
let kCGImageMetadataNamespaceExifEX: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceDublinCore: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceIPTCCore: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespacePhotoshop: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceTIFF: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceXMPBasic: CFString
@available(OSX 10.8, *)
let kCGImageMetadataNamespaceXMPRights: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixExif: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixExifAux: CFString
@available(OSX 10.9, *)
let kCGImageMetadataPrefixExifEX: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixDublinCore: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixIPTCCore: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixPhotoshop: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixTIFF: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixXMPBasic: CFString
@available(OSX 10.8, *)
let kCGImageMetadataPrefixXMPRights: CFString
enum CGImageMetadataType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case invalid
  case `default`
  case string
  case arrayUnordered
  case arrayOrdered
  case alternateArray
  case alternateText
  case structure
}
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCreate(_ xmlns: CFString, _ prefix: CFString?, _ name: CFString, _ type: CGImageMetadataType, _ value: CFTypeRef) -> CGImageMetadataTag?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCopyNamespace(_ tag: CGImageMetadataTag) -> CFString?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCopyPrefix(_ tag: CGImageMetadataTag) -> CFString?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCopyName(_ tag: CGImageMetadataTag) -> CFString?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCopyValue(_ tag: CGImageMetadataTag) -> CFTypeRef?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagGetType(_ tag: CGImageMetadataTag) -> CGImageMetadataType
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataTagCopyQualifiers(_ tag: CGImageMetadataTag) -> CFArray?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCopyTags(_ metadata: CGImageMetadata) -> CFArray?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCopyTagWithPath(_ metadata: CGImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> CGImageMetadataTag?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCopyStringValueWithPath(_ metadata: CGImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> CFString?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataRegisterNamespaceForPrefix(_ metadata: CGMutableImageMetadata, _ xmlns: CFString, _ prefix: CFString, _ err: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataSetTagWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString, _ tag: CGImageMetadataTag) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataSetValueWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString, _ value: CFTypeRef) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataRemoveTagWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> Bool
typealias CGImageMetadataTagBlock = (CFString, CGImageMetadataTag) -> Bool
@available(OSX 10.8, *)
func CGImageMetadataEnumerateTagsUsingBlock(_ metadata: CGImageMetadata, _ rootPath: CFString?, _ options: CFDictionary?, _ block: CGImageMetadataTagBlock)
@available(OSX 10.8, *)
let kCGImageMetadataEnumerateRecursively: CFString
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCopyTagMatchingImageProperty(_ metadata: CGImageMetadata, _ dictionaryName: CFString, _ propertyName: CFString) -> CGImageMetadataTag?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataSetValueMatchingImageProperty(_ metadata: CGMutableImageMetadata, _ dictionaryName: CFString, _ propertyName: CFString, _ value: CFTypeRef) -> Bool
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCreateXMPData(_ metadata: CGImageMetadata, _ options: CFDictionary?) -> CFData?
@available(OSX 10.8, *)
@discardableResult
func CGImageMetadataCreateFromXMPData(_ data: CFData) -> CGImageMetadata?
let kCFErrorDomainCGImageMetadata: CFString
enum CGImageMetadataErrors : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case unknown
  case unsupportedFormat
  case badArgument
  case conflictingArguments
  case prefixConflict
}

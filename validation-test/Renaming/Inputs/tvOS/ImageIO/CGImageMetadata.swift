
class CGImageMetadata {
}
@discardableResult
func CGImageMetadataGetTypeID() -> CFTypeID
class CGMutableImageMetadata {
}
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCreateMutable() -> CGMutableImageMetadata
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCreateMutableCopy(_ metadata: CGImageMetadata) -> CGMutableImageMetadata?
class CGImageMetadataTag {
}
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagGetTypeID() -> CFTypeID
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceExif: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceExifAux: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceExifEX: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceDublinCore: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceIPTCCore: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespacePhotoshop: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceTIFF: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceXMPBasic: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataNamespaceXMPRights: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixExif: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixExifAux: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixExifEX: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixDublinCore: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixIPTCCore: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixPhotoshop: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixTIFF: CFString
@available(tvOS 7.0, *)
let kCGImageMetadataPrefixXMPBasic: CFString
@available(tvOS 7.0, *)
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
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCreate(_ xmlns: CFString, _ prefix: CFString?, _ name: CFString, _ type: CGImageMetadataType, _ value: CFTypeRef) -> CGImageMetadataTag?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCopyNamespace(_ tag: CGImageMetadataTag) -> CFString?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCopyPrefix(_ tag: CGImageMetadataTag) -> CFString?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCopyName(_ tag: CGImageMetadataTag) -> CFString?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCopyValue(_ tag: CGImageMetadataTag) -> CFTypeRef?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagGetType(_ tag: CGImageMetadataTag) -> CGImageMetadataType
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataTagCopyQualifiers(_ tag: CGImageMetadataTag) -> CFArray?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCopyTags(_ metadata: CGImageMetadata) -> CFArray?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCopyTagWithPath(_ metadata: CGImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> CGImageMetadataTag?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCopyStringValueWithPath(_ metadata: CGImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> CFString?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataRegisterNamespaceForPrefix(_ metadata: CGMutableImageMetadata, _ xmlns: CFString, _ prefix: CFString, _ err: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataSetTagWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString, _ tag: CGImageMetadataTag) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataSetValueWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString, _ value: CFTypeRef) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataRemoveTagWithPath(_ metadata: CGMutableImageMetadata, _ parent: CGImageMetadataTag?, _ path: CFString) -> Bool
typealias CGImageMetadataTagBlock = (CFString, CGImageMetadataTag) -> Bool
@available(tvOS 7.0, *)
func CGImageMetadataEnumerateTagsUsingBlock(_ metadata: CGImageMetadata, _ rootPath: CFString?, _ options: CFDictionary?, _ block: CGImageMetadataTagBlock)
@available(tvOS 7.0, *)
let kCGImageMetadataEnumerateRecursively: CFString
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCopyTagMatchingImageProperty(_ metadata: CGImageMetadata, _ dictionaryName: CFString, _ propertyName: CFString) -> CGImageMetadataTag?
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataSetValueMatchingImageProperty(_ metadata: CGMutableImageMetadata, _ dictionaryName: CFString, _ propertyName: CFString, _ value: CFTypeRef) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func CGImageMetadataCreateXMPData(_ metadata: CGImageMetadata, _ options: CFDictionary?) -> CFData?
@available(tvOS 7.0, *)
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

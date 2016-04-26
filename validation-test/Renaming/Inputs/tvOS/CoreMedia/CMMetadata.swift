
var kCMMetadataIdentifierError_AllocationFailed: OSStatus { get }
var kCMMetadataIdentifierError_RequiredParameterMissing: OSStatus { get }
var kCMMetadataIdentifierError_BadKey: OSStatus { get }
var kCMMetadataIdentifierError_BadKeyLength: OSStatus { get }
var kCMMetadataIdentifierError_BadKeyType: OSStatus { get }
var kCMMetadataIdentifierError_BadNumberKey: OSStatus { get }
var kCMMetadataIdentifierError_BadKeySpace: OSStatus { get }
var kCMMetadataIdentifierError_BadIdentifier: OSStatus { get }
var kCMMetadataIdentifierError_NoKeyValueAvailable: OSStatus { get }
var kCMMetadataDataTypeRegistryError_AllocationFailed: OSStatus { get }
var kCMMetadataDataTypeRegistryError_RequiredParameterMissing: OSStatus { get }
var kCMMetadataDataTypeRegistryError_BadDataTypeIdentifier: OSStatus { get }
var kCMMetadataDataTypeRegistryError_DataTypeAlreadyRegistered: OSStatus { get }
var kCMMetadataDataTypeRegistryError_RequiresConformingBaseType: OSStatus { get }
var kCMMetadataDataTypeRegistryError_MultipleConformingBaseTypes: OSStatus { get }
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_QuickTimeUserData: CFString
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_ISOUserData: CFString
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_QuickTimeMetadata: CFString
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_iTunes: CFString
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_ID3: CFString
@available(tvOS 8.0, *)
let kCMMetadataKeySpace_Icy: CFString
@available(tvOS 9.3, *)
let kCMMetadataKeySpace_HLSDateRange: CFString
@available(tvOS 8.0, *)
let kCMMetadataIdentifier_QuickTimeMetadataLocation_ISO6709: CFString
@available(tvOS 8.0, *)
let kCMMetadataIdentifier_QuickTimeMetadataDirection_Facing: CFString
@available(tvOS 8.0, *)
let kCMMetadataIdentifier_QuickTimeMetadataPreferredAffineTransform: CFString
@available(tvOS 9.0, *)
let kCMMetadataIdentifier_QuickTimeMetadataVideoOrientation: CFString
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataCreateIdentifierForKeyAndKeySpace(_ allocator: CFAllocator?, _ key: CFTypeRef, _ keySpace: CFString, _ identifierOut: UnsafeMutablePointer<CFString?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataCreateKeyFromIdentifier(_ allocator: CFAllocator?, _ identifier: CFString, _ keyOut: UnsafeMutablePointer<CFTypeRef?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataCreateKeyFromIdentifierAsCFData(_ allocator: CFAllocator?, _ identifier: CFString, _ keyOut: UnsafeMutablePointer<CFData?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataCreateKeySpaceFromIdentifier(_ allocator: CFAllocator?, _ identifier: CFString, _ keySpaceOut: UnsafeMutablePointer<CFString?>) -> OSStatus
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_RawData: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UTF8: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UTF16: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_GIF: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_JPEG: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_PNG: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_BMP: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_Float32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_Float64: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_SInt8: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_SInt16: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_SInt32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_SInt64: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UInt8: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UInt16: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UInt32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_UInt64: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_PointF32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_DimensionsF32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_RectF32: CFString
@available(tvOS 8.0, *)
let kCMMetadataBaseDataType_AffineTransformF64: CFString
@available(tvOS 9.0, *)
let kCMMetadataBaseDataType_PolygonF32: CFString
@available(tvOS 9.0, *)
let kCMMetadataBaseDataType_PolylineF32: CFString
@available(tvOS 9.0, *)
let kCMMetadataBaseDataType_JSON: CFString
@available(tvOS 8.0, *)
let kCMMetadataDataType_QuickTimeMetadataLocation_ISO6709: CFString
@available(tvOS 8.0, *)
let kCMMetadataDataType_QuickTimeMetadataDirection: CFString
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryRegisterDataType(_ dataType: CFString, _ description: CFString, _ conformingDataTypes: CFArray) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryDataTypeIsRegistered(_ dataType: CFString) -> Bool
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryGetDataTypeDescription(_ dataType: CFString) -> CFString
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryGetConformingDataTypes(_ dataType: CFString) -> CFArray
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryDataTypeConformsToDataType(_ dataType: CFString, _ conformsToDataType: CFString) -> Bool
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryGetBaseDataTypes() -> CFArray?
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryDataTypeIsBaseDataType(_ dataType: CFString) -> Bool
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataDataTypeRegistryGetBaseDataTypeForConformingDataType(_ dataType: CFString) -> CFString

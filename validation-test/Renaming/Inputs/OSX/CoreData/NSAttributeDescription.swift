
enum NSAttributeType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case undefinedAttributeType
  case integer16AttributeType
  case integer32AttributeType
  case integer64AttributeType
  case decimalAttributeType
  case doubleAttributeType
  case floatAttributeType
  case stringAttributeType
  case booleanAttributeType
  case dateAttributeType
  case binaryDataAttributeType
  @available(OSX 10.5, *)
  case transformableAttributeType
  @available(OSX 10.6, *)
  case objectIDAttributeType
}
@available(OSX 10.4, *)
class NSAttributeDescription : NSPropertyDescription {
  var attributeType: NSAttributeType
  var attributeValueClassName: String?
  var defaultValue: AnyObject?
  @available(OSX 10.5, *)
  var valueTransformerName: String?
  @available(OSX 10.7, *)
  var allowsExternalBinaryDataStorage: Bool
}
struct __attributeDescriptionFlags {
  var _hasMaxValueInExtraIvars: UInt32
  var _hasMinValueInExtraIvars: UInt32
  var _storeBinaryDataExternally: UInt32
  var _reservedAttributeDescription: UInt32
  init()
  init(_hasMaxValueInExtraIvars _hasMaxValueInExtraIvars: UInt32, _hasMinValueInExtraIvars _hasMinValueInExtraIvars: UInt32, _storeBinaryDataExternally _storeBinaryDataExternally: UInt32, _reservedAttributeDescription _reservedAttributeDescription: UInt32)
}

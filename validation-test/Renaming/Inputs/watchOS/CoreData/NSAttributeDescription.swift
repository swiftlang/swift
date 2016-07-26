
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
  @available(watchOS 2.0, *)
  case transformableAttributeType
  @available(watchOS 2.0, *)
  case objectIDAttributeType
}
@available(watchOS 2.0, *)
class NSAttributeDescription : NSPropertyDescription {
  var attributeType: NSAttributeType
  var attributeValueClassName: String?
  var defaultValue: AnyObject?
  @available(watchOS 2.0, *)
  var valueTransformerName: String?
  @available(watchOS 2.0, *)
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

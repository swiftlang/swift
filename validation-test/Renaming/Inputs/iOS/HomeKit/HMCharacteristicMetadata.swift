
@available(iOS 8.0, *)
class HMCharacteristicMetadata : NSObject {
  var minimumValue: NSNumber? { get }
  var maximumValue: NSNumber? { get }
  var stepValue: NSNumber? { get }
  var maxLength: NSNumber? { get }
  var format: String? { get }
  var units: String? { get }
  var manufacturerDescription: String? { get }
}
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatBool: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatInt: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatFloat: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatString: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatArray: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatDictionary: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatUInt8: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatUInt16: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatUInt32: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatUInt64: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatData: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataFormatTLV8: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataUnitsCelsius: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataUnitsFahrenheit: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataUnitsPercentage: String
@available(iOS 8.0, *)
let HMCharacteristicMetadataUnitsArcDegree: String
@available(iOS 8.3, *)
let HMCharacteristicMetadataUnitsSeconds: String
@available(iOS 9.3, *)
let HMCharacteristicMetadataUnitsLux: String

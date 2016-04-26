
@available(watchOS 20000, *)
class HMCharacteristicMetadata : NSObject {
  var minimumValue: NSNumber? { get }
  var maximumValue: NSNumber? { get }
  var stepValue: NSNumber? { get }
  var maxLength: NSNumber? { get }
  var format: String? { get }
  var units: String? { get }
  var manufacturerDescription: String? { get }
}
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatBool: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatInt: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatFloat: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatString: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatArray: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatDictionary: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatUInt8: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatUInt16: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatUInt32: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatUInt64: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatData: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataFormatTLV8: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataUnitsCelsius: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataUnitsFahrenheit: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataUnitsPercentage: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataUnitsArcDegree: String
@available(watchOS 20000, *)
let HMCharacteristicMetadataUnitsSeconds: String
@available(watchOS 2.2, *)
let HMCharacteristicMetadataUnitsLux: String


let CBUUIDCharacteristicExtendedPropertiesString: String
let CBUUIDCharacteristicUserDescriptionString: String
let CBUUIDClientCharacteristicConfigurationString: String
let CBUUIDServerCharacteristicConfigurationString: String
let CBUUIDCharacteristicFormatString: String
let CBUUIDCharacteristicAggregateFormatString: String
let CBUUIDValidRangeString: String
let CBUUIDGenericAccessProfileString: String
let CBUUIDGenericAttributeProfileString: String
let CBUUIDDeviceNameString: String
let CBUUIDAppearanceString: String
let CBUUIDPeripheralPrivacyFlagString: String
let CBUUIDReconnectionAddressString: String
let CBUUIDPeripheralPreferredConnectionParametersString: String
let CBUUIDServiceChangedString: String
@available(OSX 10.7, *)
class CBUUID : NSObject, NSCopying {
  var data: NSData { get }
  @available(OSX 10.10, *)
  var uuidString: String { get }
  /*not inherited*/ init(string theString: String)
  /*not inherited*/ init(data theData: NSData)
  /*not inherited*/ init(cfuuid theUUID: CFUUID)
  @available(OSX 10.9, *)
  /*not inherited*/ init(nsuuid theUUID: NSUUID)
}


typealias AudioObjectID = UInt32
typealias AudioClassID = UInt32
typealias AudioObjectPropertySelector = UInt32
typealias AudioObjectPropertyScope = UInt32
typealias AudioObjectPropertyElement = UInt32
struct AudioObjectPropertyAddress {
  var mSelector: AudioObjectPropertySelector
  var mScope: AudioObjectPropertyScope
  var mElement: AudioObjectPropertyElement
  init()
  init(mSelector mSelector: AudioObjectPropertySelector, mScope mScope: AudioObjectPropertyScope, mElement mElement: AudioObjectPropertyElement)
}
var kAudioHardwareNoError: OSStatus { get }
var kAudioHardwareNotRunningError: OSStatus { get }
var kAudioHardwareUnspecifiedError: OSStatus { get }
var kAudioHardwareUnknownPropertyError: OSStatus { get }
var kAudioHardwareBadPropertySizeError: OSStatus { get }
var kAudioHardwareIllegalOperationError: OSStatus { get }
var kAudioHardwareBadObjectError: OSStatus { get }
var kAudioHardwareBadDeviceError: OSStatus { get }
var kAudioHardwareBadStreamError: OSStatus { get }
var kAudioHardwareUnsupportedOperationError: OSStatus { get }
var kAudioDeviceUnsupportedFormatError: OSStatus { get }
var kAudioDevicePermissionsError: OSStatus { get }
var kAudioObjectUnknown: AudioObjectID { get }
var kAudioObjectPropertyScopeGlobal: AudioObjectPropertyScope { get }
var kAudioObjectPropertyScopeInput: AudioObjectPropertyScope { get }
var kAudioObjectPropertyScopeOutput: AudioObjectPropertyScope { get }
var kAudioObjectPropertyScopePlayThrough: AudioObjectPropertyScope { get }
var kAudioObjectPropertyElementMaster: AudioObjectPropertyScope { get }
var kAudioObjectPropertySelectorWildcard: AudioObjectPropertySelector { get }
var kAudioObjectPropertyScopeWildcard: AudioObjectPropertyScope { get }
var kAudioObjectPropertyElementWildcard: AudioObjectPropertyElement { get }
var kAudioObjectClassIDWildcard: AudioClassID { get }
var kAudioObjectClassID: AudioClassID { get }
var kAudioObjectPropertyBaseClass: AudioObjectPropertySelector { get }
var kAudioObjectPropertyClass: AudioObjectPropertySelector { get }
var kAudioObjectPropertyOwner: AudioObjectPropertySelector { get }
var kAudioObjectPropertyName: AudioObjectPropertySelector { get }
var kAudioObjectPropertyModelName: AudioObjectPropertySelector { get }
var kAudioObjectPropertyManufacturer: AudioObjectPropertySelector { get }
var kAudioObjectPropertyElementName: AudioObjectPropertySelector { get }
var kAudioObjectPropertyElementCategoryName: AudioObjectPropertySelector { get }
var kAudioObjectPropertyElementNumberName: AudioObjectPropertySelector { get }
var kAudioObjectPropertyOwnedObjects: AudioObjectPropertySelector { get }
var kAudioObjectPropertyIdentify: AudioObjectPropertySelector { get }
var kAudioObjectPropertySerialNumber: AudioObjectPropertySelector { get }
var kAudioObjectPropertyFirmwareVersion: AudioObjectPropertySelector { get }
var kAudioPlugInClassID: AudioClassID { get }
var kAudioPlugInPropertyBundleID: AudioObjectPropertySelector { get }
var kAudioPlugInPropertyDeviceList: AudioObjectPropertySelector { get }
var kAudioPlugInPropertyTranslateUIDToDevice: AudioObjectPropertySelector { get }
var kAudioPlugInPropertyBoxList: AudioObjectPropertySelector { get }
var kAudioPlugInPropertyTranslateUIDToBox: AudioObjectPropertySelector { get }
var kAudioTransportManagerClassID: AudioClassID { get }
var kAudioTransportManagerPropertyEndPointList: AudioObjectPropertySelector { get }
var kAudioTransportManagerPropertyTranslateUIDToEndPoint: AudioObjectPropertySelector { get }
var kAudioTransportManagerPropertyTransportType: AudioObjectPropertySelector { get }
var kAudioBoxClassID: AudioClassID { get }
var kAudioBoxPropertyBoxUID: AudioObjectPropertySelector { get }
var kAudioBoxPropertyTransportType: AudioObjectPropertySelector { get }
var kAudioBoxPropertyHasAudio: AudioObjectPropertySelector { get }
var kAudioBoxPropertyHasVideo: AudioObjectPropertySelector { get }
var kAudioBoxPropertyHasMIDI: AudioObjectPropertySelector { get }
var kAudioBoxPropertyIsProtected: AudioObjectPropertySelector { get }
var kAudioBoxPropertyAcquired: AudioObjectPropertySelector { get }
var kAudioBoxPropertyAcquisitionFailed: AudioObjectPropertySelector { get }
var kAudioBoxPropertyDeviceList: AudioObjectPropertySelector { get }
var kAudioDeviceClassID: AudioClassID { get }
var kAudioDeviceTransportTypeUnknown: UInt32 { get }
var kAudioDeviceTransportTypeBuiltIn: UInt32 { get }
var kAudioDeviceTransportTypeAggregate: UInt32 { get }
var kAudioDeviceTransportTypeVirtual: UInt32 { get }
var kAudioDeviceTransportTypePCI: UInt32 { get }
var kAudioDeviceTransportTypeUSB: UInt32 { get }
var kAudioDeviceTransportTypeFireWire: UInt32 { get }
var kAudioDeviceTransportTypeBluetooth: UInt32 { get }
var kAudioDeviceTransportTypeBluetoothLE: UInt32 { get }
var kAudioDeviceTransportTypeHDMI: UInt32 { get }
var kAudioDeviceTransportTypeDisplayPort: UInt32 { get }
var kAudioDeviceTransportTypeAirPlay: UInt32 { get }
var kAudioDeviceTransportTypeAVB: UInt32 { get }
var kAudioDeviceTransportTypeThunderbolt: UInt32 { get }
var kAudioDevicePropertyConfigurationApplication: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceUID: AudioObjectPropertySelector { get }
var kAudioDevicePropertyModelUID: AudioObjectPropertySelector { get }
var kAudioDevicePropertyTransportType: AudioObjectPropertySelector { get }
var kAudioDevicePropertyRelatedDevices: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClockDomain: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceIsAlive: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceIsRunning: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceCanBeDefaultDevice: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceCanBeDefaultSystemDevice: AudioObjectPropertySelector { get }
var kAudioDevicePropertyLatency: AudioObjectPropertySelector { get }
var kAudioDevicePropertyStreams: AudioObjectPropertySelector { get }
var kAudioObjectPropertyControlList: AudioObjectPropertySelector { get }
var kAudioDevicePropertySafetyOffset: AudioObjectPropertySelector { get }
var kAudioDevicePropertyNominalSampleRate: AudioObjectPropertySelector { get }
var kAudioDevicePropertyAvailableNominalSampleRates: AudioObjectPropertySelector { get }
var kAudioDevicePropertyIcon: AudioObjectPropertySelector { get }
var kAudioDevicePropertyIsHidden: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPreferredChannelsForStereo: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPreferredChannelLayout: AudioObjectPropertySelector { get }
var kAudioEndPointDeviceClassID: AudioClassID { get }
var kAudioEndPointDeviceUIDKey: String { get }
var kAudioEndPointDeviceNameKey: String { get }
var kAudioEndPointDeviceEndPointListKey: String { get }
var kAudioEndPointDeviceMasterEndPointKey: String { get }
var kAudioEndPointDeviceIsPrivateKey: String { get }
var kAudioEndPointDevicePropertyComposition: AudioObjectPropertySelector { get }
var kAudioEndPointDevicePropertyEndPointList: AudioObjectPropertySelector { get }
var kAudioEndPointDevicePropertyIsPrivate: AudioObjectPropertySelector { get }
var kAudioEndPointClassID: AudioClassID { get }
var kAudioEndPointUIDKey: String { get }
var kAudioEndPointNameKey: String { get }
var kAudioEndPointInputChannelsKey: String { get }
var kAudioEndPointOutputChannelsKey: String { get }
struct AudioStreamRangedDescription {
  var mFormat: AudioStreamBasicDescription
  var mSampleRateRange: AudioValueRange
  init()
  init(mFormat mFormat: AudioStreamBasicDescription, mSampleRateRange mSampleRateRange: AudioValueRange)
}
var kAudioStreamClassID: AudioClassID { get }
var kAudioStreamTerminalTypeUnknown: UInt32 { get }
var kAudioStreamTerminalTypeLine: UInt32 { get }
var kAudioStreamTerminalTypeDigitalAudioInterface: UInt32 { get }
var kAudioStreamTerminalTypeSpeaker: UInt32 { get }
var kAudioStreamTerminalTypeHeadphones: UInt32 { get }
var kAudioStreamTerminalTypeLFESpeaker: UInt32 { get }
var kAudioStreamTerminalTypeReceiverSpeaker: UInt32 { get }
var kAudioStreamTerminalTypeMicrophone: UInt32 { get }
var kAudioStreamTerminalTypeHeadsetMicrophone: UInt32 { get }
var kAudioStreamTerminalTypeReceiverMicrophone: UInt32 { get }
var kAudioStreamTerminalTypeTTY: UInt32 { get }
var kAudioStreamTerminalTypeHDMI: UInt32 { get }
var kAudioStreamTerminalTypeDisplayPort: UInt32 { get }
var kAudioStreamPropertyIsActive: AudioObjectPropertySelector { get }
var kAudioStreamPropertyDirection: AudioObjectPropertySelector { get }
var kAudioStreamPropertyTerminalType: AudioObjectPropertySelector { get }
var kAudioStreamPropertyStartingChannel: AudioObjectPropertySelector { get }
var kAudioStreamPropertyLatency: AudioObjectPropertySelector { get }
var kAudioStreamPropertyVirtualFormat: AudioObjectPropertySelector { get }
var kAudioStreamPropertyAvailableVirtualFormats: AudioObjectPropertySelector { get }
var kAudioStreamPropertyPhysicalFormat: AudioObjectPropertySelector { get }
var kAudioStreamPropertyAvailablePhysicalFormats: AudioObjectPropertySelector { get }
var kAudioControlClassID: AudioClassID { get }
var kAudioControlPropertyScope: AudioObjectPropertySelector { get }
var kAudioControlPropertyElement: AudioObjectPropertySelector { get }
var kAudioSliderControlClassID: AudioClassID { get }
var kAudioSliderControlPropertyValue: AudioObjectPropertySelector { get }
var kAudioSliderControlPropertyRange: AudioObjectPropertySelector { get }
var kAudioLevelControlClassID: AudioClassID { get }
var kAudioVolumeControlClassID: AudioClassID { get }
var kAudioLFEVolumeControlClassID: AudioClassID { get }
var kAudioLevelControlPropertyScalarValue: AudioObjectPropertySelector { get }
var kAudioLevelControlPropertyDecibelValue: AudioObjectPropertySelector { get }
var kAudioLevelControlPropertyDecibelRange: AudioObjectPropertySelector { get }
var kAudioLevelControlPropertyConvertScalarToDecibels: AudioObjectPropertySelector { get }
var kAudioLevelControlPropertyConvertDecibelsToScalar: AudioObjectPropertySelector { get }
var kAudioBooleanControlClassID: AudioClassID { get }
var kAudioMuteControlClassID: AudioClassID { get }
var kAudioSoloControlClassID: AudioClassID { get }
var kAudioJackControlClassID: AudioClassID { get }
var kAudioLFEMuteControlClassID: AudioClassID { get }
var kAudioPhantomPowerControlClassID: AudioClassID { get }
var kAudioPhaseInvertControlClassID: AudioClassID { get }
var kAudioClipLightControlClassID: AudioClassID { get }
var kAudioTalkbackControlClassID: AudioClassID { get }
var kAudioListenbackControlClassID: AudioClassID { get }
var kAudioBooleanControlPropertyValue: AudioObjectPropertySelector { get }
var kAudioSelectorControlClassID: AudioClassID { get }
var kAudioDataSourceControlClassID: AudioClassID { get }
var kAudioDataDestinationControlClassID: AudioClassID { get }
var kAudioClockSourceControlClassID: AudioClassID { get }
var kAudioLineLevelControlClassID: AudioClassID { get }
var kAudioHighPassFilterControlClassID: AudioClassID { get }
var kAudioSelectorControlPropertyCurrentItem: AudioObjectPropertySelector { get }
var kAudioSelectorControlPropertyAvailableItems: AudioObjectPropertySelector { get }
var kAudioSelectorControlPropertyItemName: AudioObjectPropertySelector { get }
var kAudioSelectorControlPropertyItemKind: AudioObjectPropertySelector { get }
var kAudioSelectorControlItemKindSpacer: UInt32 { get }
var kAudioClockSourceItemKindInternal: UInt32 { get }
var kAudioStereoPanControlClassID: AudioClassID { get }
var kAudioStereoPanControlPropertyValue: AudioObjectPropertySelector { get }
var kAudioStereoPanControlPropertyPanningChannels: AudioObjectPropertySelector { get }

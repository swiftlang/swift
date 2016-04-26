
class SCNetworkInterface {
}
@available(OSX 10.4, *)
let kSCNetworkInterfaceType6to4: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeBluetooth: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeBond: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeEthernet: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeFireWire: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeIEEE80211: CFString
@available(OSX 10.5, *)
let kSCNetworkInterfaceTypeIPSec: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeIrDA: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeL2TP: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeModem: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypePPP: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypePPTP: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeSerial: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeVLAN: CFString
@available(OSX 10.5, *)
let kSCNetworkInterfaceTypeWWAN: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceTypeIPv4: CFString
@available(OSX 10.4, *)
let kSCNetworkInterfaceIPv4: SCNetworkInterface
typealias SCBondInterface = SCNetworkInterface
class SCBondStatus {
}
var kSCBondStatusOK: Int { get }
var kSCBondStatusLinkInvalid: Int { get }
var kSCBondStatusNoPartner: Int { get }
var kSCBondStatusNotInActiveGroup: Int { get }
var kSCBondStatusUnknown: Int { get }
@available(OSX 10.4, *)
let kSCBondStatusDeviceAggregationStatus: CFString
@available(OSX 10.4, *)
let kSCBondStatusDeviceCollecting: CFString
@available(OSX 10.4, *)
let kSCBondStatusDeviceDistributing: CFString
typealias SCVLANInterface = SCNetworkInterface
class SCNetworkProtocol {
}
@available(OSX 10.4, *)
let kSCNetworkProtocolTypeDNS: CFString
@available(OSX 10.4, *)
let kSCNetworkProtocolTypeIPv4: CFString
@available(OSX 10.4, *)
let kSCNetworkProtocolTypeIPv6: CFString
@available(OSX 10.4, *)
let kSCNetworkProtocolTypeProxies: CFString
@available(OSX 10.5, *)
let kSCNetworkProtocolTypeSMB: CFString
class SCNetworkService {
}
class SCNetworkSet {
}
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceCopyAll() -> CFArray
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetSupportedInterfaceTypes(_ interface: SCNetworkInterface) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetSupportedProtocolTypes(_ interface: SCNetworkInterface) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceCreateWithInterface(_ interface: SCNetworkInterface, _ interfaceType: CFString) -> SCNetworkInterface?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetBSDName(_ interface: SCNetworkInterface) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetConfiguration(_ interface: SCNetworkInterface) -> CFDictionary?
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceGetExtendedConfiguration(_ interface: SCNetworkInterface, _ extendedType: CFString) -> CFDictionary?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetHardwareAddressString(_ interface: SCNetworkInterface) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetInterface(_ interface: SCNetworkInterface) -> SCNetworkInterface?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetInterfaceType(_ interface: SCNetworkInterface) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceGetLocalizedDisplayName(_ interface: SCNetworkInterface) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkInterfaceSetConfiguration(_ interface: SCNetworkInterface, _ config: CFDictionary) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceSetExtendedConfiguration(_ interface: SCNetworkInterface, _ extendedType: CFString, _ config: CFDictionary) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceCopyMediaOptions(_ interface: SCNetworkInterface, _ current: UnsafeMutablePointer<Unmanaged<CFDictionary>?>?, _ active: UnsafeMutablePointer<Unmanaged<CFDictionary>?>?, _ available: UnsafeMutablePointer<Unmanaged<CFArray>?>?, _ filter: Bool) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceCopyMediaSubTypes(_ available: CFArray) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceCopyMediaSubTypeOptions(_ available: CFArray, _ subType: CFString) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceCopyMTU(_ interface: SCNetworkInterface, _ mtu_cur: UnsafeMutablePointer<Int32>?, _ mtu_min: UnsafeMutablePointer<Int32>?, _ mtu_max: UnsafeMutablePointer<Int32>?) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceSetMediaOptions(_ interface: SCNetworkInterface, _ subtype: CFString, _ options: CFArray) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceSetMTU(_ interface: SCNetworkInterface, _ mtu: Int32) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkInterfaceForceConfigurationRefresh(_ interface: SCNetworkInterface) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceCopyAll(_ prefs: SCPreferences) -> CFArray
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceCopyAvailableMemberInterfaces(_ prefs: SCPreferences) -> CFArray
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceCreate(_ prefs: SCPreferences) -> SCBondInterface?
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceRemove(_ bond: SCBondInterface) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceGetMemberInterfaces(_ bond: SCBondInterface) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceGetOptions(_ bond: SCBondInterface) -> CFDictionary?
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceSetMemberInterfaces(_ bond: SCBondInterface, _ members: CFArray) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceSetLocalizedDisplayName(_ bond: SCBondInterface, _ newName: CFString) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceSetOptions(_ bond: SCBondInterface, _ newOptions: CFDictionary) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCBondInterfaceCopyStatus(_ bond: SCBondInterface) -> SCBondStatus?
@available(OSX 10.5, *)
@discardableResult
func SCBondStatusGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func SCBondStatusGetMemberInterfaces(_ bondStatus: SCBondStatus) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func SCBondStatusGetInterfaceStatus(_ bondStatus: SCBondStatus, _ interface: SCNetworkInterface?) -> CFDictionary?
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceCopyAll(_ prefs: SCPreferences) -> CFArray
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceCopyAvailablePhysicalInterfaces() -> CFArray
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceCreate(_ prefs: SCPreferences, _ physical: SCNetworkInterface, _ tag: CFNumber) -> SCVLANInterface?
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceRemove(_ vlan: SCVLANInterface) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceGetPhysicalInterface(_ vlan: SCVLANInterface) -> SCNetworkInterface?
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceGetTag(_ vlan: SCVLANInterface) -> CFNumber?
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceGetOptions(_ vlan: SCVLANInterface) -> CFDictionary?
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceSetPhysicalInterfaceAndTag(_ vlan: SCVLANInterface, _ physical: SCNetworkInterface, _ tag: CFNumber) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceSetLocalizedDisplayName(_ vlan: SCVLANInterface, _ newName: CFString) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCVLANInterfaceSetOptions(_ vlan: SCVLANInterface, _ newOptions: CFDictionary) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolGetConfiguration(_ protocol: SCNetworkProtocol) -> CFDictionary?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolGetEnabled(_ protocol: SCNetworkProtocol) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolGetProtocolType(_ protocol: SCNetworkProtocol) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolSetConfiguration(_ protocol: SCNetworkProtocol, _ config: CFDictionary) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkProtocolSetEnabled(_ protocol: SCNetworkProtocol, _ enabled: Bool) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceAddProtocolType(_ service: SCNetworkService, _ protocolType: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceCopyAll(_ prefs: SCPreferences) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceCopyProtocols(_ service: SCNetworkService) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceCreate(_ prefs: SCPreferences, _ interface: SCNetworkInterface) -> SCNetworkService?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceCopy(_ prefs: SCPreferences, _ serviceID: CFString) -> SCNetworkService?
@available(OSX 10.5, *)
@discardableResult
func SCNetworkServiceEstablishDefaultConfiguration(_ service: SCNetworkService) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceGetEnabled(_ service: SCNetworkService) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceGetInterface(_ service: SCNetworkService) -> SCNetworkInterface?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceGetName(_ service: SCNetworkService) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceCopyProtocol(_ service: SCNetworkService, _ protocolType: CFString) -> SCNetworkProtocol?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceGetServiceID(_ service: SCNetworkService) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceRemove(_ service: SCNetworkService) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceRemoveProtocolType(_ service: SCNetworkService, _ protocolType: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceSetEnabled(_ service: SCNetworkService, _ enabled: Bool) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkServiceSetName(_ service: SCNetworkService, _ name: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetAddService(_ set: SCNetworkSet, _ service: SCNetworkService) -> Bool
@available(OSX 10.5, *)
@discardableResult
func SCNetworkSetContainsInterface(_ set: SCNetworkSet, _ interface: SCNetworkInterface) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetCopyAll(_ prefs: SCPreferences) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetCopyCurrent(_ prefs: SCPreferences) -> SCNetworkSet?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetCopyServices(_ set: SCNetworkSet) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetCreate(_ prefs: SCPreferences) -> SCNetworkSet?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetCopy(_ prefs: SCPreferences, _ setID: CFString) -> SCNetworkSet?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetGetName(_ set: SCNetworkSet) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetGetSetID(_ set: SCNetworkSet) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetGetServiceOrder(_ set: SCNetworkSet) -> CFArray?
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetRemove(_ set: SCNetworkSet) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetRemoveService(_ set: SCNetworkSet, _ service: SCNetworkService) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetSetCurrent(_ set: SCNetworkSet) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetSetName(_ set: SCNetworkSet, _ name: CFString) -> Bool
@available(OSX 10.4, *)
@discardableResult
func SCNetworkSetSetServiceOrder(_ set: SCNetworkSet, _ newOrder: CFArray) -> Bool

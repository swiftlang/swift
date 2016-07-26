
typealias MIDISetupRef = MIDIObjectRef
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceAddEntity(_ device: MIDIDeviceRef, _ name: CFString, _ embedded: Bool, _ numSourceEndpoints: Int, _ numDestinationEndpoints: Int, _ newEntity: UnsafeMutablePointer<MIDIEntityRef>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceRemoveEntity(_ device: MIDIDeviceRef, _ entity: MIDIEntityRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityAddOrRemoveEndpoints(_ entity: MIDIEntityRef, _ numSourceEndpoints: Int, _ numDestinationEndpoints: Int) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISetupAddDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISetupRemoveDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISetupAddExternalDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISetupRemoveExternalDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIExternalDeviceCreate(_ name: CFString, _ manufacturer: CFString, _ model: CFString, _ outDevice: UnsafeMutablePointer<MIDIDeviceRef>) -> OSStatus

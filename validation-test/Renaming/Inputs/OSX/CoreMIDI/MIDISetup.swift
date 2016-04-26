
typealias MIDISetupRef = MIDIObjectRef
@available(OSX 10.0, *)
@discardableResult
func MIDIDeviceAddEntity(_ device: MIDIDeviceRef, _ name: CFString, _ embedded: Bool, _ numSourceEndpoints: Int, _ numDestinationEndpoints: Int, _ newEntity: UnsafeMutablePointer<MIDIEntityRef>) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIDeviceRemoveEntity(_ device: MIDIDeviceRef, _ entity: MIDIEntityRef) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIEntityAddOrRemoveEndpoints(_ entity: MIDIEntityRef, _ numSourceEndpoints: Int, _ numDestinationEndpoints: Int) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDISetupAddDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDISetupRemoveDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDISetupAddExternalDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDISetupRemoveExternalDevice(_ device: MIDIDeviceRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIExternalDeviceCreate(_ name: CFString, _ manufacturer: CFString, _ model: CFString, _ outDevice: UnsafeMutablePointer<MIDIDeviceRef>) -> OSStatus

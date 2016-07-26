
let kMAAudibleMediaSettingsChangedNotification: CFString
@available(OSX 10.10, *)
let MAMediaCharacteristicDescribesVideoForAccessibility: CFString
@available(OSX 10.10, *)
@discardableResult
func MAAudibleMediaCopyPreferredCharacteristics() -> Unmanaged<CFArray>

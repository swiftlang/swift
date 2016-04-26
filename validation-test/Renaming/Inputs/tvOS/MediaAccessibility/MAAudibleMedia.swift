
let kMAAudibleMediaSettingsChangedNotification: CFString
@available(tvOS 8.0, *)
let MAMediaCharacteristicDescribesVideoForAccessibility: CFString
@available(tvOS 8.0, *)
@discardableResult
func MAAudibleMediaCopyPreferredCharacteristics() -> Unmanaged<CFArray>

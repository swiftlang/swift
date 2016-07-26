
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesSetComputerName(_ prefs: SCPreferences, _ name: CFString, _ nameEncoding: CFStringEncoding) -> Bool
@available(OSX 10.2, *)
@discardableResult
func SCPreferencesSetLocalHostName(_ prefs: SCPreferences, _ name: CFString) -> Bool

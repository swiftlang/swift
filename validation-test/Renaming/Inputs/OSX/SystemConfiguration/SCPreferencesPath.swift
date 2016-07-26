
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathCreateUniqueChild(_ prefs: SCPreferences, _ prefix: CFString) -> CFString?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathGetValue(_ prefs: SCPreferences, _ path: CFString) -> CFDictionary?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathGetLink(_ prefs: SCPreferences, _ path: CFString) -> CFString?
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathSetValue(_ prefs: SCPreferences, _ path: CFString, _ value: CFDictionary) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathSetLink(_ prefs: SCPreferences, _ path: CFString, _ link: CFString) -> Bool
@available(OSX 10.1, *)
@discardableResult
func SCPreferencesPathRemoveValue(_ prefs: SCPreferences, _ path: CFString) -> Bool

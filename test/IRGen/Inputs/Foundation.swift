// This is an overlay Swift module.
@exported import Foundation

@exported import ObjectiveC

// String/NSString bridging functions.
@asmname("swift_StringToNSString") internal
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString") internal
func _convertNSStringToString(nsstring: NSString) -> String


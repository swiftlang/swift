// This is an overlay Swift module.
@exported import Foundation

@exported import ObjectiveC

// String/NSString bridging functions.
@asmname("swift_StringToNSString")
func _convertStringToNSString(string: String) -> NSString

@asmname("swift_NSStringToString")
func _convertNSStringToString(nsstring: NSString) -> String


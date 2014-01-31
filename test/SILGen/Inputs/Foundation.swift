// Fake Foundation module for testing String/NSString bridging.

@exported import ObjectiveC
@exported import Foundation // clang module

@asmname="swift_StringToNSString"
func convertStringToNSString(string: @inout String) -> NSString

@asmname="swift_NSStringToString"
func convertNSStringToString(nsstring: NSString, string: @inout String)



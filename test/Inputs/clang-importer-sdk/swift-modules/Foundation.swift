@exported import ObjectiveC
@exported import Foundation

@asmname="swift_StringToNSString"
func convertStringToNSString(string: @inout String) -> NSString

@asmname="swift_NSStringToString"
func convertNSStringToString(nsstring: NSString, string: @inout String)

struct NSZone {
  var pointer : COpaquePointer
}

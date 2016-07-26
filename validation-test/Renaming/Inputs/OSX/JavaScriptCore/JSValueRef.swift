
struct JSType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kJSTypeUndefined: JSType { get }
var kJSTypeNull: JSType { get }
var kJSTypeBoolean: JSType { get }
var kJSTypeNumber: JSType { get }
var kJSTypeString: JSType { get }
var kJSTypeObject: JSType { get }
@discardableResult
func JSValueGetType(_ ctx: JSContextRef!, _ _: JSValueRef!) -> JSType
@discardableResult
func JSValueIsUndefined(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsNull(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsBoolean(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsNumber(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsString(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsObject(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsObjectOfClass(_ ctx: JSContextRef!, _ value: JSValueRef!, _ jsClass: JSClassRef!) -> Bool
@available(OSX 10.11, *)
@discardableResult
func JSValueIsArray(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@available(OSX 10.11, *)
@discardableResult
func JSValueIsDate(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueIsEqual(_ ctx: JSContextRef!, _ a: JSValueRef!, _ b: JSValueRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> Bool
@discardableResult
func JSValueIsStrictEqual(_ ctx: JSContextRef!, _ a: JSValueRef!, _ b: JSValueRef!) -> Bool
@discardableResult
func JSValueIsInstanceOfConstructor(_ ctx: JSContextRef!, _ value: JSValueRef!, _ constructor: JSObjectRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> Bool
@discardableResult
func JSValueMakeUndefined(_ ctx: JSContextRef!) -> JSValueRef!
@discardableResult
func JSValueMakeNull(_ ctx: JSContextRef!) -> JSValueRef!
@discardableResult
func JSValueMakeBoolean(_ ctx: JSContextRef!, _ boolean: Bool) -> JSValueRef!
@discardableResult
func JSValueMakeNumber(_ ctx: JSContextRef!, _ number: Double) -> JSValueRef!
@discardableResult
func JSValueMakeString(_ ctx: JSContextRef!, _ string: JSStringRef!) -> JSValueRef!
@available(OSX 10.7, *)
@discardableResult
func JSValueMakeFromJSONString(_ ctx: JSContextRef!, _ string: JSStringRef!) -> JSValueRef!
@available(OSX 10.7, *)
@discardableResult
func JSValueCreateJSONString(_ ctx: JSContextRef!, _ value: JSValueRef!, _ indent: UInt32, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSStringRef!
@discardableResult
func JSValueToBoolean(_ ctx: JSContextRef!, _ value: JSValueRef!) -> Bool
@discardableResult
func JSValueToNumber(_ ctx: JSContextRef!, _ value: JSValueRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> Double
@discardableResult
func JSValueToStringCopy(_ ctx: JSContextRef!, _ value: JSValueRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSStringRef!
@discardableResult
func JSValueToObject(_ ctx: JSContextRef!, _ value: JSValueRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
func JSValueProtect(_ ctx: JSContextRef!, _ value: JSValueRef!)
func JSValueUnprotect(_ ctx: JSContextRef!, _ value: JSValueRef!)

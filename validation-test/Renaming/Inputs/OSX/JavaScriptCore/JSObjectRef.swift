
var kJSPropertyAttributeNone: Int { get }
var kJSPropertyAttributeReadOnly: Int { get }
var kJSPropertyAttributeDontEnum: Int { get }
var kJSPropertyAttributeDontDelete: Int { get }
typealias JSPropertyAttributes = UInt32
var kJSClassAttributeNone: Int { get }
var kJSClassAttributeNoAutomaticPrototype: Int { get }
typealias JSClassAttributes = UInt32
typealias JSObjectInitializeCallback = @convention(c) (JSContextRef!, JSObjectRef!) -> Void
typealias JSObjectFinalizeCallback = @convention(c) (JSObjectRef!) -> Void
typealias JSObjectHasPropertyCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSStringRef!) -> Bool
typealias JSObjectGetPropertyCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSStringRef!, UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
typealias JSObjectSetPropertyCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSStringRef!, JSValueRef!, UnsafeMutablePointer<JSValueRef?>!) -> Bool
typealias JSObjectDeletePropertyCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSStringRef!, UnsafeMutablePointer<JSValueRef?>!) -> Bool
typealias JSObjectGetPropertyNamesCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSPropertyNameAccumulatorRef!) -> Void
typealias JSObjectCallAsFunctionCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSObjectRef!, Int, UnsafePointer<JSValueRef?>!, UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
typealias JSObjectCallAsConstructorCallback = @convention(c) (JSContextRef!, JSObjectRef!, Int, UnsafePointer<JSValueRef?>!, UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
typealias JSObjectHasInstanceCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSValueRef!, UnsafeMutablePointer<JSValueRef?>!) -> Bool
typealias JSObjectConvertToTypeCallback = @convention(c) (JSContextRef!, JSObjectRef!, JSType, UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
struct JSStaticValue {
  var name: UnsafePointer<Int8>!
  var getProperty: JSObjectGetPropertyCallback!
  var setProperty: JSObjectSetPropertyCallback!
  var attributes: JSPropertyAttributes
  init()
  init(name name: UnsafePointer<Int8>!, getProperty getProperty: JSObjectGetPropertyCallback!, setProperty setProperty: JSObjectSetPropertyCallback!, attributes attributes: JSPropertyAttributes)
}
struct JSStaticFunction {
  var name: UnsafePointer<Int8>!
  var callAsFunction: JSObjectCallAsFunctionCallback!
  var attributes: JSPropertyAttributes
  init()
  init(name name: UnsafePointer<Int8>!, callAsFunction callAsFunction: JSObjectCallAsFunctionCallback!, attributes attributes: JSPropertyAttributes)
}
struct JSClassDefinition {
  var version: Int32
  var attributes: JSClassAttributes
  var className: UnsafePointer<Int8>!
  var parentClass: JSClassRef!
  var staticValues: UnsafePointer<JSStaticValue>!
  var staticFunctions: UnsafePointer<JSStaticFunction>!
  var initialize: JSObjectInitializeCallback!
  var finalize: JSObjectFinalizeCallback!
  var hasProperty: JSObjectHasPropertyCallback!
  var getProperty: JSObjectGetPropertyCallback!
  var setProperty: JSObjectSetPropertyCallback!
  var deleteProperty: JSObjectDeletePropertyCallback!
  var getPropertyNames: JSObjectGetPropertyNamesCallback!
  var callAsFunction: JSObjectCallAsFunctionCallback!
  var callAsConstructor: JSObjectCallAsConstructorCallback!
  var hasInstance: JSObjectHasInstanceCallback!
  var convertToType: JSObjectConvertToTypeCallback!
  init()
  init(version version: Int32, attributes attributes: JSClassAttributes, className className: UnsafePointer<Int8>!, parentClass parentClass: JSClassRef!, staticValues staticValues: UnsafePointer<JSStaticValue>!, staticFunctions staticFunctions: UnsafePointer<JSStaticFunction>!, initialize initialize: JSObjectInitializeCallback!, finalize finalize: JSObjectFinalizeCallback!, hasProperty hasProperty: JSObjectHasPropertyCallback!, getProperty getProperty: JSObjectGetPropertyCallback!, setProperty setProperty: JSObjectSetPropertyCallback!, deleteProperty deleteProperty: JSObjectDeletePropertyCallback!, getPropertyNames getPropertyNames: JSObjectGetPropertyNamesCallback!, callAsFunction callAsFunction: JSObjectCallAsFunctionCallback!, callAsConstructor callAsConstructor: JSObjectCallAsConstructorCallback!, hasInstance hasInstance: JSObjectHasInstanceCallback!, convertToType convertToType: JSObjectConvertToTypeCallback!)
}
let kJSClassDefinitionEmpty: JSClassDefinition
@discardableResult
func JSClassCreate(_ definition: UnsafePointer<JSClassDefinition>!) -> JSClassRef!
@discardableResult
func JSClassRetain(_ jsClass: JSClassRef!) -> JSClassRef!
func JSClassRelease(_ jsClass: JSClassRef!)
@discardableResult
func JSObjectMake(_ ctx: JSContextRef!, _ jsClass: JSClassRef!, _ data: UnsafeMutablePointer<Void>!) -> JSObjectRef!
@discardableResult
func JSObjectMakeFunctionWithCallback(_ ctx: JSContextRef!, _ name: JSStringRef!, _ callAsFunction: JSObjectCallAsFunctionCallback!) -> JSObjectRef!
@discardableResult
func JSObjectMakeConstructor(_ ctx: JSContextRef!, _ jsClass: JSClassRef!, _ callAsConstructor: JSObjectCallAsConstructorCallback!) -> JSObjectRef!
@available(OSX 10.6, *)
@discardableResult
func JSObjectMakeArray(_ ctx: JSContextRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@available(OSX 10.6, *)
@discardableResult
func JSObjectMakeDate(_ ctx: JSContextRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@available(OSX 10.6, *)
@discardableResult
func JSObjectMakeError(_ ctx: JSContextRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@available(OSX 10.6, *)
@discardableResult
func JSObjectMakeRegExp(_ ctx: JSContextRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@discardableResult
func JSObjectMakeFunction(_ ctx: JSContextRef!, _ name: JSStringRef!, _ parameterCount: UInt32, _ parameterNames: UnsafePointer<JSStringRef?>!, _ body: JSStringRef!, _ sourceURL: JSStringRef!, _ startingLineNumber: Int32, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@discardableResult
func JSObjectGetPrototype(_ ctx: JSContextRef!, _ object: JSObjectRef!) -> JSValueRef!
func JSObjectSetPrototype(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ value: JSValueRef!)
@discardableResult
func JSObjectHasProperty(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyName: JSStringRef!) -> Bool
@discardableResult
func JSObjectGetProperty(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyName: JSStringRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
func JSObjectSetProperty(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyName: JSStringRef!, _ value: JSValueRef!, _ attributes: JSPropertyAttributes, _ exception: UnsafeMutablePointer<JSValueRef?>!)
@discardableResult
func JSObjectDeleteProperty(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyName: JSStringRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> Bool
@discardableResult
func JSObjectGetPropertyAtIndex(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyIndex: UInt32, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
func JSObjectSetPropertyAtIndex(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ propertyIndex: UInt32, _ value: JSValueRef!, _ exception: UnsafeMutablePointer<JSValueRef?>!)
@discardableResult
func JSObjectGetPrivate(_ object: JSObjectRef!) -> UnsafeMutablePointer<Void>!
@discardableResult
func JSObjectSetPrivate(_ object: JSObjectRef!, _ data: UnsafeMutablePointer<Void>!) -> Bool
@discardableResult
func JSObjectIsFunction(_ ctx: JSContextRef!, _ object: JSObjectRef!) -> Bool
@discardableResult
func JSObjectCallAsFunction(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ thisObject: JSObjectRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
@discardableResult
func JSObjectIsConstructor(_ ctx: JSContextRef!, _ object: JSObjectRef!) -> Bool
@discardableResult
func JSObjectCallAsConstructor(_ ctx: JSContextRef!, _ object: JSObjectRef!, _ argumentCount: Int, _ arguments: UnsafePointer<JSValueRef?>!, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSObjectRef!
@discardableResult
func JSObjectCopyPropertyNames(_ ctx: JSContextRef!, _ object: JSObjectRef!) -> JSPropertyNameArrayRef!
@discardableResult
func JSPropertyNameArrayRetain(_ array: JSPropertyNameArrayRef!) -> JSPropertyNameArrayRef!
func JSPropertyNameArrayRelease(_ array: JSPropertyNameArrayRef!)
@discardableResult
func JSPropertyNameArrayGetCount(_ array: JSPropertyNameArrayRef!) -> Int
@discardableResult
func JSPropertyNameArrayGetNameAtIndex(_ array: JSPropertyNameArrayRef!, _ index: Int) -> JSStringRef!
func JSPropertyNameAccumulatorAddName(_ accumulator: JSPropertyNameAccumulatorRef!, _ propertyName: JSStringRef!)

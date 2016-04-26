
@available(OSX 10.9, *)
class ODModuleEntry : NSObject {
  @available(OSX 10.9, *)
  var mappings: ODMappings!
  @available(OSX 10.9, *)
  var supportedOptions: [AnyObject]! { get }
  @available(OSX 10.9, *)
  var name: String!
  @available(OSX 10.9, *)
  var xpcServiceName: String!
  @available(OSX 10.9, *)
  var uuidString: String!
  @available(OSX 10.9, *)
  convenience init!(name name: String!, xpcServiceName xpcServiceName: String!)
  @available(OSX 10.9, *)
  func setOption(_ optionName: String!, value value: AnyObject!)
  @available(OSX 10.9, *)
  @discardableResult
  func option(_ optionName: String!) -> AnyObject!
}

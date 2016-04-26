
@available(tvOS 7.0, *)
class JSVirtualMachine : NSObject {
  func addManagedReference(_ object: AnyObject!, withOwner owner: AnyObject!)
  func removeManagedReference(_ object: AnyObject!, withOwner owner: AnyObject!)
}

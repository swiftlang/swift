
@available(iOS 7.0, *)
class JSManagedValue : NSObject {
  @available(iOS 8.0, *)
  /*not inherited*/ init!(value value: JSValue!, andOwner owner: AnyObject!)
  init!(value value: JSValue!)
  var value: JSValue! { get }
}

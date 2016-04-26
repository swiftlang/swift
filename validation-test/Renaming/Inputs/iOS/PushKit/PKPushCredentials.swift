
@available(iOS 8.0, *)
class PKPushCredentials : NSObject {
  var type: String! { get }
  @NSCopying var token: NSData! { get }
}

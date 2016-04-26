
@available(iOS 8.0, *)
class PKPushPayload : NSObject {
  var type: String! { get }
  var dictionaryPayload: [NSObject : AnyObject]! { get }
}

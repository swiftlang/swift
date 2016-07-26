
@available(iOS 9.0, *)
class NSDataAsset : NSObject, NSCopying {
  convenience init?(name name: String)
  init?(name name: String, bundle bundle: NSBundle)
  var name: String { get }
  @NSCopying var data: NSData { get }
  var typeIdentifier: String { get }
}

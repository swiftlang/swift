
@available(iOS 4.0, *)
class UINib : NSObject {
  /*not inherited*/ init(nibName name: String, bundle bundleOrNil: NSBundle?)
  /*not inherited*/ init(data data: NSData, bundle bundleOrNil: NSBundle?)
  @discardableResult
  func instantiate(withOwner ownerOrNil: AnyObject?, options optionsOrNil: [NSObject : AnyObject]? = [:]) -> [AnyObject]
}

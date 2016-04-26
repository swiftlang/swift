
@available(iOS 8.0, *)
class HMTrigger : NSObject {
  var name: String { get }
  var isEnabled: Bool { get }
  var actionSets: [HMActionSet] { get }
  @NSCopying var lastFireDate: NSDate? { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  func updateName(_ name: String, completionHandler completion: (NSError?) -> Void)
  func addActionSet(_ actionSet: HMActionSet, completionHandler completion: (NSError?) -> Void)
  func removeActionSet(_ actionSet: HMActionSet, completionHandler completion: (NSError?) -> Void)
  func enable(_ enable: Bool, completionHandler completion: (NSError?) -> Void)
}

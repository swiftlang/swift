
@available(iOS 8.0, *)
class HMHomeManager : NSObject {
  weak var delegate: @sil_weak HMHomeManagerDelegate?
  var primaryHome: HMHome? { get }
  var homes: [HMHome] { get }
  func updatePrimaryHome(_ home: HMHome, completionHandler completion: (NSError?) -> Void)
  func removeHome(_ home: HMHome, completionHandler completion: (NSError?) -> Void)
}
@available(iOS 8.0, *)
protocol HMHomeManagerDelegate : NSObjectProtocol {
  optional func homeManagerDidUpdateHomes(_ manager: HMHomeManager)
  optional func homeManagerDidUpdatePrimaryHome(_ manager: HMHomeManager)
  optional func homeManager(_ manager: HMHomeManager, didAdd home: HMHome)
  optional func homeManager(_ manager: HMHomeManager, didRemove home: HMHome)
}

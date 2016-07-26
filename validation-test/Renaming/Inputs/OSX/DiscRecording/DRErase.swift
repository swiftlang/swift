
class DRErase : NSObject {
  /*not inherited*/ init!(for device: DRDevice!)
  init!(device device: DRDevice!)
  func start()
  @discardableResult
  func status() -> [NSObject : AnyObject]!
  @discardableResult
  func properties() -> [NSObject : AnyObject]!
  func setProperties(_ properties: [NSObject : AnyObject]!)
  @discardableResult
  func device() -> DRDevice!
}
extension DRErase {
  @discardableResult
  func eraseType() -> String!
  func setEraseType(_ type: String!)
}
@available(OSX 10.2, *)
let DREraseTypeKey: String
@available(OSX 10.2, *)
let DREraseTypeQuick: String
@available(OSX 10.2, *)
let DREraseTypeComplete: String
@available(OSX 10.2, *)
let DREraseStatusChangedNotification: String

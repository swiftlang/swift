
class DRMSF : NSNumber {
  init!(frames frames: UInt32)
  init!(string string: String!)
  @discardableResult
  func minutes() -> UInt32
  @discardableResult
  func seconds() -> UInt32
  @discardableResult
  func frames() -> UInt32
  @discardableResult
  func sectors() -> UInt32
  @discardableResult
  func adding(_ msf: DRMSF!) -> DRMSF!
  @discardableResult
  func subtracting(_ msf: DRMSF!) -> DRMSF!
  @discardableResult
  func description(withFormat format: String!) -> String!
  @discardableResult
  func isEqual(to otherDRMSF: DRMSF!) -> Bool
}


class DRFolder : DRFSObject {
  init!(path path: String!)
}
extension DRFolder {
  @discardableResult
  class func virtualFolder(withName name: String!) -> DRFolder!
  init!(name name: String!)
  func makeVirtual()
  func addChild(_ child: DRFSObject!)
  func removeChild(_ child: DRFSObject!)
  @discardableResult
  func count() -> Int
  @discardableResult
  func children() -> [AnyObject]!
}

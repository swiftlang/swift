
class NSArchiver : NSCoder {
  init(forWritingWith mdata: NSMutableData)
  var archiverData: NSMutableData { get }
  @discardableResult
  class func archivedData(withRootObject rootObject: AnyObject) -> NSData
  @discardableResult
  class func archiveRootObject(_ rootObject: AnyObject, toFile path: String) -> Bool
  func encodeClassName(_ trueName: String, intoClassName inArchiveName: String)
  @discardableResult
  func classNameEncoded(forTrueClassName trueName: String) -> String?
  func replace(_ object: AnyObject, with newObject: AnyObject)
}
class NSUnarchiver : NSCoder {
  init?(forReadingWith data: NSData)
  var isAtEnd: Bool { get }
  @discardableResult
  class func unarchiveObject(with data: NSData) -> AnyObject?
  @discardableResult
  class func unarchiveObject(withFile path: String) -> AnyObject?
  class func decodeClassName(_ inArchiveName: String, asClassName trueName: String)
  func decodeClassName(_ inArchiveName: String, asClassName trueName: String)
  @discardableResult
  class func classNameDecoded(forArchiveClassName inArchiveName: String) -> String
  @discardableResult
  func classNameDecoded(forArchiveClassName inArchiveName: String) -> String
  func replace(_ object: AnyObject, with newObject: AnyObject)
}
let NSInconsistentArchiveException: String
extension NSObject {
  var classForArchiver: AnyClass? { get }
  @discardableResult
  class func replacementObject(for archiver: NSArchiver) -> AnyObject?
  @discardableResult
  func replacementObject(for archiver: NSArchiver) -> AnyObject?
  class func classForArchiver() -> AnyClass?
}

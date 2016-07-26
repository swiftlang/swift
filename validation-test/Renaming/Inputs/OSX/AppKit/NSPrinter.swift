
enum NSPrinterTableStatus : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case OK
  case notFound
  case error
}
class NSPrinter : NSObject, NSCopying, NSCoding {
  @discardableResult
  class func printerNames() -> [String]
  @discardableResult
  class func printerTypes() -> [String]
  /*not inherited*/ init?(name name: String)
  /*not inherited*/ init?(type type: String)
  var name: String { get }
  var type: String { get }
  var languageLevel: Int { get }
  @discardableResult
  func pageSize(forPaper paperName: String) -> NSSize
  var deviceDescription: [String : AnyObject] { get }
}
extension NSPrinter {
}

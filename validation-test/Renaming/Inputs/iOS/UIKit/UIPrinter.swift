
@available(iOS 8.0, *)
class UIPrinter : NSObject {
  /*not inherited*/ init(url url: NSURL)
  @NSCopying var url: NSURL { get }
  var displayName: String { get }
  var displayLocation: String? { get }
  var supportedJobTypes: UIPrinterJobTypes { get }
  var makeAndModel: String? { get }
  var supportsColor: Bool { get }
  var supportsDuplex: Bool { get }
  func contactPrinter(_ completionHandler: ((Bool) -> Void)? = nil)
}
@available(iOS 8.0, *)
struct UIPrinterJobTypes : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var document: UIPrinterJobTypes { get }
  static var envelope: UIPrinterJobTypes { get }
  static var label: UIPrinterJobTypes { get }
  static var photo: UIPrinterJobTypes { get }
  static var receipt: UIPrinterJobTypes { get }
  static var roll: UIPrinterJobTypes { get }
  static var largeFormat: UIPrinterJobTypes { get }
  static var postcard: UIPrinterJobTypes { get }
}

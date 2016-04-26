
@available(OSX 10.9, *)
class NSPDFInfo : NSObject, NSCopying, NSCoding {
  @NSCopying var url: NSURL?
  var isFileExtensionHidden: Bool
  var tagNames: [String]
  var orientation: NSPaperOrientation
  var paperSize: NSSize
  var attributes: NSMutableDictionary { get }
}

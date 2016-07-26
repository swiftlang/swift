
@available(OSX 10.6, *)
let NSPasteboardTypeString: String
@available(OSX 10.6, *)
let NSPasteboardTypePDF: String
@available(OSX 10.6, *)
let NSPasteboardTypeTIFF: String
@available(OSX 10.6, *)
let NSPasteboardTypePNG: String
@available(OSX 10.6, *)
let NSPasteboardTypeRTF: String
@available(OSX 10.6, *)
let NSPasteboardTypeRTFD: String
@available(OSX 10.6, *)
let NSPasteboardTypeHTML: String
@available(OSX 10.6, *)
let NSPasteboardTypeTabularText: String
@available(OSX 10.6, *)
let NSPasteboardTypeFont: String
@available(OSX 10.6, *)
let NSPasteboardTypeRuler: String
@available(OSX 10.6, *)
let NSPasteboardTypeColor: String
@available(OSX 10.6, *)
let NSPasteboardTypeSound: String
@available(OSX 10.6, *)
let NSPasteboardTypeMultipleTextSelection: String
@available(OSX 10.7, *)
let NSPasteboardTypeTextFinderOptions: String
let NSGeneralPboard: String
let NSFontPboard: String
let NSRulerPboard: String
let NSFindPboard: String
let NSDragPboard: String
class NSPasteboard : NSObject {
  @discardableResult
  class func general() -> NSPasteboard
  /*not inherited*/ init(name name: String)
  @discardableResult
  class func withUniqueName() -> NSPasteboard
  var name: String { get }
  var changeCount: Int { get }
  func releaseGlobally()
  @available(OSX 10.6, *)
  @discardableResult
  func clearContents() -> Int
  @available(OSX 10.6, *)
  @discardableResult
  func writeObjects(_ objects: [NSPasteboardWriting]) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  func readObjects(forClasses classArray: [AnyClass], options options: [String : AnyObject]? = [:]) -> [AnyObject]?
  @available(OSX 10.6, *)
  var pasteboardItems: [NSPasteboardItem]? { get }
  @available(OSX 10.6, *)
  @discardableResult
  func index(of pasteboardItem: NSPasteboardItem) -> Int
  @available(OSX 10.6, *)
  @discardableResult
  func canReadItemWithDataConforming(toTypes types: [String]) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  func canReadObject(forClasses classArray: [AnyClass], options options: [String : AnyObject]? = [:]) -> Bool
  @discardableResult
  func declareTypes(_ newTypes: [String], owner newOwner: AnyObject?) -> Int
  @discardableResult
  func addTypes(_ newTypes: [String], owner newOwner: AnyObject?) -> Int
  var types: [String]? { get }
  @discardableResult
  func availableType(from types: [String]) -> String?
  @discardableResult
  func setData(_ data: NSData?, forType dataType: String) -> Bool
  @discardableResult
  func setPropertyList(_ plist: AnyObject, forType dataType: String) -> Bool
  @discardableResult
  func setString(_ string: String, forType dataType: String) -> Bool
  @discardableResult
  func data(forType dataType: String) -> NSData?
  @discardableResult
  func propertyList(forType dataType: String) -> AnyObject?
  @discardableResult
  func string(forType dataType: String) -> String?
}
extension NSPasteboard {
  @discardableResult
  class func typesFilterable(to type: String) -> [String]
  /*not inherited*/ init(byFilteringFile filename: String)
  /*not inherited*/ init(byFilteringData data: NSData, ofType type: String)
  /*not inherited*/ init(byFilteringTypesIn pboard: NSPasteboard)
}
extension NSObject {
  class func pasteboard(_ sender: NSPasteboard, provideDataForType type: String)
  func pasteboard(_ sender: NSPasteboard, provideDataForType type: String)
  class func pasteboardChangedOwner(_ sender: NSPasteboard)
  func pasteboardChangedOwner(_ sender: NSPasteboard)
}
@available(OSX 10.6, *)
let NSPasteboardURLReadingFileURLsOnlyKey: String
@available(OSX 10.6, *)
let NSPasteboardURLReadingContentsConformToTypesKey: String
@available(OSX 10.6, *)
struct NSPasteboardWritingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var promised: NSPasteboardWritingOptions { get }
}
protocol NSPasteboardWriting : NSObjectProtocol {
  @discardableResult
  func writableTypes(for pasteboard: NSPasteboard) -> [String]
  @available(OSX 10.6, *)
  @discardableResult
  optional func writingOptions(forType type: String, pasteboard pasteboard: NSPasteboard) -> NSPasteboardWritingOptions
  @discardableResult
  func pasteboardPropertyList(forType type: String) -> AnyObject?
}
@available(OSX 10.6, *)
struct NSPasteboardReadingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var asString: NSPasteboardReadingOptions { get }
  static var asPropertyList: NSPasteboardReadingOptions { get }
  static var asKeyedArchive: NSPasteboardReadingOptions { get }
}
protocol NSPasteboardReading : NSObjectProtocol {
  @discardableResult
  static func readableTypes(for pasteboard: NSPasteboard) -> [String]
  @available(OSX 10.6, *)
  @discardableResult
  optional static func readingOptions(forType type: String, pasteboard pasteboard: NSPasteboard) -> NSPasteboardReadingOptions
  init?(pasteboardPropertyList propertyList: AnyObject, ofType type: String)
}
extension NSURL : NSPasteboardWriting, NSPasteboardReading {
  /*not inherited*/ init?(from pasteBoard: NSPasteboard)
  func write(to pasteBoard: NSPasteboard)
}
extension NSString : NSPasteboardWriting, NSPasteboardReading {
}
let NSStringPboardType: String
let NSFilenamesPboardType: String
let NSTIFFPboardType: String
let NSRTFPboardType: String
let NSTabularTextPboardType: String
let NSFontPboardType: String
let NSRulerPboardType: String
let NSColorPboardType: String
let NSRTFDPboardType: String
let NSHTMLPboardType: String
let NSURLPboardType: String
let NSPDFPboardType: String
@available(OSX 10.5, *)
let NSMultipleTextSelectionPboardType: String
let NSPostScriptPboardType: String
let NSVCardPboardType: String
let NSInkTextPboardType: String
let NSFilesPromisePboardType: String
@available(OSX 10.6, *)
let NSPasteboardTypeFindPanelSearchOptions: String
extension NSPasteboard {
  @discardableResult
  func writeFileContents(_ filename: String) -> Bool
  @discardableResult
  func readFileContentsType(_ type: String?, toFile filename: String) -> String?
  @discardableResult
  func write(_ wrapper: NSFileWrapper) -> Bool
  @discardableResult
  func readFileWrapper() -> NSFileWrapper?
}
let NSFileContentsPboardType: String
@discardableResult
func NSCreateFilenamePboardType(_ fileType: String) -> String?
@discardableResult
func NSCreateFileContentsPboardType(_ fileType: String) -> String?
@discardableResult
func NSGetFileType(_ pboardType: String) -> String?
@discardableResult
func NSGetFileTypes(_ pboardTypes: [String]) -> [String]?

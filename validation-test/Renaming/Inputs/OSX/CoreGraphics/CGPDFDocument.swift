
class CGPDFDocument {
}
extension CGPDFDocument {
  @available(OSX 10.0, *)
  /*not inherited*/ init?(_ provider: CGDataProvider?)
  @available(OSX 10.0, *)
  /*not inherited*/ init?(with url: CFURL?)
  @available(OSX 10.3, *)
  func getVersion(majorVersion majorVersion: UnsafeMutablePointer<Int32>, minorVersion minorVersion: UnsafeMutablePointer<Int32>)
  @available(OSX 10.2, *)
  var isEncrypted: Bool { get }
  @available(OSX 10.2, *)
  @discardableResult
  func unlockWithPassword(_ password: UnsafePointer<Int8>) -> Bool
  @available(OSX 10.2, *)
  var isUnlocked: Bool { get }
  @available(OSX 10.2, *)
  var allowsPrinting: Bool { get }
  @available(OSX 10.2, *)
  var allowsCopying: Bool { get }
  @available(OSX 10.0, *)
  var numberOfPages: Int { get }
  @available(OSX 10.3, *)
  @discardableResult
  func page(AtIndex pageNumber: Int) -> CGPDFPage?
  @available(OSX 10.3, *)
  var catalog: CGPDFDictionaryRef? { get }
  @available(OSX 10.4, *)
  var info: CGPDFDictionaryRef? { get }
  @available(OSX 10.4, *)
  var fileIdentifier: CGPDFArrayRef? { get }
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
}

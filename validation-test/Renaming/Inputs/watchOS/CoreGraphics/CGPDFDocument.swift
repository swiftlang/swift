
class CGPDFDocument {
}
extension CGPDFDocument {
  @available(watchOS 2.0, *)
  /*not inherited*/ init?(_ provider: CGDataProvider?)
  @available(watchOS 2.0, *)
  /*not inherited*/ init?(with url: CFURL?)
  @available(watchOS 2.0, *)
  func getVersion(majorVersion majorVersion: UnsafeMutablePointer<Int32>, minorVersion minorVersion: UnsafeMutablePointer<Int32>)
  @available(watchOS 2.0, *)
  var isEncrypted: Bool { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func unlockWithPassword(_ password: UnsafePointer<Int8>) -> Bool
  @available(watchOS 2.0, *)
  var isUnlocked: Bool { get }
  @available(watchOS 2.0, *)
  var allowsPrinting: Bool { get }
  @available(watchOS 2.0, *)
  var allowsCopying: Bool { get }
  @available(watchOS 2.0, *)
  var numberOfPages: Int { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func page(AtIndex pageNumber: Int) -> CGPDFPage?
  @available(watchOS 2.0, *)
  var catalog: CGPDFDictionaryRef? { get }
  @available(watchOS 2.0, *)
  var info: CGPDFDictionaryRef? { get }
  @available(watchOS 2.0, *)
  var fileIdentifier: CGPDFArrayRef? { get }
  @available(watchOS 2.0, *)
  class var typeID: CFTypeID { get }
}

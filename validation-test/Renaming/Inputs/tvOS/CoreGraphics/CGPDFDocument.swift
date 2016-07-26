
class CGPDFDocument {
}
extension CGPDFDocument {
  @available(tvOS 2.0, *)
  /*not inherited*/ init?(_ provider: CGDataProvider?)
  @available(tvOS 2.0, *)
  /*not inherited*/ init?(with url: CFURL?)
  @available(tvOS 2.0, *)
  func getVersion(majorVersion majorVersion: UnsafeMutablePointer<Int32>, minorVersion minorVersion: UnsafeMutablePointer<Int32>)
  @available(tvOS 2.0, *)
  var isEncrypted: Bool { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func unlockWithPassword(_ password: UnsafePointer<Int8>) -> Bool
  @available(tvOS 2.0, *)
  var isUnlocked: Bool { get }
  @available(tvOS 2.0, *)
  var allowsPrinting: Bool { get }
  @available(tvOS 2.0, *)
  var allowsCopying: Bool { get }
  @available(tvOS 2.0, *)
  var numberOfPages: Int { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func page(AtIndex pageNumber: Int) -> CGPDFPage?
  @available(tvOS 2.0, *)
  var catalog: CGPDFDictionaryRef? { get }
  @available(tvOS 2.0, *)
  var info: CGPDFDictionaryRef? { get }
  @available(tvOS 2.0, *)
  var fileIdentifier: CGPDFArrayRef? { get }
  @available(tvOS 2.0, *)
  class var typeID: CFTypeID { get }
}

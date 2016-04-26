
class CGPDFPage {
}
enum CGPDFBox : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case mediaBox
  case cropBox
  case bleedBox
  case trimBox
  case artBox
}
extension CGPDFPage {
  @available(OSX 10.3, *)
  var document: CGPDFDocument? { get }
  @available(OSX 10.3, *)
  var pageNumber: Int { get }
  @available(OSX 10.3, *)
  var boxRect: CGRect { get }
  @available(OSX 10.3, *)
  var rotationAngle: Int32 { get }
  @available(OSX 10.3, *)
  @discardableResult
  func getDrawingTransform(_ box: CGPDFBox, rect rect: CGRect, rotate rotate: Int32, preserveAspectRatio preserveAspectRatio: Bool) -> CGAffineTransform
  @available(OSX 10.3, *)
  var dictionary: CGPDFDictionaryRef? { get }
  @available(OSX 10.3, *)
  class var typeID: CFTypeID { get }
}

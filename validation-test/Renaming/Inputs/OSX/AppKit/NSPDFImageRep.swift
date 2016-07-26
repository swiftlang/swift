
class NSPDFImageRep : NSImageRep {
  init?(data pdfData: NSData)
  var pdfRepresentation: NSData { get }
  var bounds: NSRect { get }
  var currentPage: Int
  var pageCount: Int { get }
}

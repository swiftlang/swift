
class NSCIImageRep : NSImageRep {
  init(ciImage image: CIImage)
  var ciImage: CIImage { get }
}
extension CIImage {
  init?(bitmapImageRep bitmapImageRep: NSBitmapImageRep)
  func draw(in rect: NSRect, from fromRect: NSRect, operation op: NSCompositingOperation, fraction delta: CGFloat)
  func draw(at point: NSPoint, from fromRect: NSRect, operation op: NSCompositingOperation, fraction delta: CGFloat)
}

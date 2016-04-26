
class NSEPSImageRep : NSImageRep {
  init?(data epsData: NSData)
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func prepareGState()
  @NSCopying var epsRepresentation: NSData { get }
  var boundingBox: NSRect { get }
}

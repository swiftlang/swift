
class NSPICTImageRep : NSImageRep {
  init?(data pictData: NSData)
  @NSCopying var pictRepresentation: NSData { get }
  var boundingBox: NSRect { get }
}

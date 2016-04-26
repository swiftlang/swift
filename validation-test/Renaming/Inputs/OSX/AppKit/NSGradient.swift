
typealias NSGradientDrawingOptions = Int
var NSGradientDrawsBeforeStartingLocation: Int { get }
var NSGradientDrawsAfterEndingLocation: Int { get }
@available(OSX 10.5, *)
class NSGradient : NSObject, NSCopying, NSCoding {
  convenience init?(starting startingColor: NSColor, ending endingColor: NSColor)
  convenience init?(colors colorArray: [NSColor])
  init?(colors colorArray: [NSColor], atLocations locations: UnsafePointer<CGFloat>?, colorSpace colorSpace: NSColorSpace)
  func draw(from startingPoint: NSPoint, to endingPoint: NSPoint, options options: NSGradientDrawingOptions)
  func draw(in rect: NSRect, angle angle: CGFloat)
  func draw(in path: NSBezierPath, angle angle: CGFloat)
  func draw(fromCenter startCenter: NSPoint, radius startRadius: CGFloat, toCenter endCenter: NSPoint, radius endRadius: CGFloat, options options: NSGradientDrawingOptions)
  func draw(in rect: NSRect, relativeCenterPosition relativeCenterPosition: NSPoint)
  func draw(in path: NSBezierPath, relativeCenterPosition relativeCenterPosition: NSPoint)
  var colorSpace: NSColorSpace { get }
  var numberOfColorStops: Int { get }
  func getColor(_ color: AutoreleasingUnsafeMutablePointer<NSColor>?, location location: UnsafeMutablePointer<CGFloat>?, at index: Int)
  @discardableResult
  func interpolatedColor(atLocation location: CGFloat) -> NSColor
}

extension NSGradient {
  convenience init?(colorsAndLocations objects: (NSColor, CGFloat)...)
}

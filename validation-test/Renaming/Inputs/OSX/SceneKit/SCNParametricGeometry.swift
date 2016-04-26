
@available(OSX 10.8, *)
class SCNPlane : SCNGeometry {
  convenience init(width width: CGFloat, height height: CGFloat)
  var width: CGFloat
  var height: CGFloat
  var widthSegmentCount: Int
  var heightSegmentCount: Int
  @available(OSX 10.9, *)
  var cornerRadius: CGFloat
  @available(OSX 10.9, *)
  var cornerSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNBox : SCNGeometry {
  convenience init(width width: CGFloat, height height: CGFloat, length length: CGFloat, chamferRadius chamferRadius: CGFloat)
  var width: CGFloat
  var height: CGFloat
  var length: CGFloat
  var chamferRadius: CGFloat
  var widthSegmentCount: Int
  var heightSegmentCount: Int
  var lengthSegmentCount: Int
  var chamferSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNPyramid : SCNGeometry {
  convenience init(width width: CGFloat, height height: CGFloat, length length: CGFloat)
  var width: CGFloat
  var height: CGFloat
  var length: CGFloat
  var widthSegmentCount: Int
  var heightSegmentCount: Int
  var lengthSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNSphere : SCNGeometry {
  convenience init(radius radius: CGFloat)
  var radius: CGFloat
  var isGeodesic: Bool
  var segmentCount: Int
}
@available(OSX 10.8, *)
class SCNCylinder : SCNGeometry {
  convenience init(radius radius: CGFloat, height height: CGFloat)
  var radius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNCone : SCNGeometry {
  convenience init(topRadius topRadius: CGFloat, bottomRadius bottomRadius: CGFloat, height height: CGFloat)
  var topRadius: CGFloat
  var bottomRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNTube : SCNGeometry {
  convenience init(innerRadius innerRadius: CGFloat, outerRadius outerRadius: CGFloat, height height: CGFloat)
  var innerRadius: CGFloat
  var outerRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNCapsule : SCNGeometry {
  convenience init(capRadius capRadius: CGFloat, height height: CGFloat)
  var capRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
  var capSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNTorus : SCNGeometry {
  convenience init(ringRadius ringRadius: CGFloat, pipeRadius pipeRadius: CGFloat)
  var ringRadius: CGFloat
  var pipeRadius: CGFloat
  var ringSegmentCount: Int
  var pipeSegmentCount: Int
}
@available(OSX 10.8, *)
class SCNFloor : SCNGeometry {
  var reflectivity: CGFloat
  var reflectionFalloffStart: CGFloat
  var reflectionFalloffEnd: CGFloat
  @available(OSX 10.10, *)
  var reflectionResolutionScaleFactor: CGFloat
}
@available(OSX 10.8, *)
class SCNText : SCNGeometry {
  convenience init(string string: AnyObject?, extrusionDepth extrusionDepth: CGFloat)
  var extrusionDepth: CGFloat
  @NSCopying var string: AnyObject?
  var font: NSFont!
  var isWrapped: Bool
  var containerFrame: CGRect
  var textSize: CGSize { get }
  var truncationMode: String
  var alignmentMode: String
  var chamferRadius: CGFloat
  @available(OSX 10.9, *)
  @NSCopying var chamferProfile: NSBezierPath?
  @available(OSX 10.9, *)
  var flatness: CGFloat
}
@available(OSX 10.9, *)
enum SCNChamferMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case both
  case front
  case back
}
@available(OSX 10.9, *)
class SCNShape : SCNGeometry {
  convenience init(path path: NSBezierPath?, extrusionDepth extrusionDepth: CGFloat)
  @NSCopying var path: NSBezierPath?
  var extrusionDepth: CGFloat
  var chamferMode: SCNChamferMode
  var chamferRadius: CGFloat
  @NSCopying var chamferProfile: NSBezierPath?
}

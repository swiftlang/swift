
@available(iOS 8.0, *)
class SCNPlane : SCNGeometry {
  convenience init(width width: CGFloat, height height: CGFloat)
  var width: CGFloat
  var height: CGFloat
  var widthSegmentCount: Int
  var heightSegmentCount: Int
  @available(iOS 8.0, *)
  var cornerRadius: CGFloat
  @available(iOS 8.0, *)
  var cornerSegmentCount: Int
}
@available(iOS 8.0, *)
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
@available(iOS 8.0, *)
class SCNPyramid : SCNGeometry {
  convenience init(width width: CGFloat, height height: CGFloat, length length: CGFloat)
  var width: CGFloat
  var height: CGFloat
  var length: CGFloat
  var widthSegmentCount: Int
  var heightSegmentCount: Int
  var lengthSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNSphere : SCNGeometry {
  convenience init(radius radius: CGFloat)
  var radius: CGFloat
  var isGeodesic: Bool
  var segmentCount: Int
}
@available(iOS 8.0, *)
class SCNCylinder : SCNGeometry {
  convenience init(radius radius: CGFloat, height height: CGFloat)
  var radius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNCone : SCNGeometry {
  convenience init(topRadius topRadius: CGFloat, bottomRadius bottomRadius: CGFloat, height height: CGFloat)
  var topRadius: CGFloat
  var bottomRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNTube : SCNGeometry {
  convenience init(innerRadius innerRadius: CGFloat, outerRadius outerRadius: CGFloat, height height: CGFloat)
  var innerRadius: CGFloat
  var outerRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNCapsule : SCNGeometry {
  convenience init(capRadius capRadius: CGFloat, height height: CGFloat)
  var capRadius: CGFloat
  var height: CGFloat
  var radialSegmentCount: Int
  var heightSegmentCount: Int
  var capSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNTorus : SCNGeometry {
  convenience init(ringRadius ringRadius: CGFloat, pipeRadius pipeRadius: CGFloat)
  var ringRadius: CGFloat
  var pipeRadius: CGFloat
  var ringSegmentCount: Int
  var pipeSegmentCount: Int
}
@available(iOS 8.0, *)
class SCNFloor : SCNGeometry {
  var reflectivity: CGFloat
  var reflectionFalloffStart: CGFloat
  var reflectionFalloffEnd: CGFloat
  @available(iOS 8.0, *)
  var reflectionResolutionScaleFactor: CGFloat
}
@available(iOS 8.0, *)
class SCNText : SCNGeometry {
  convenience init(string string: AnyObject?, extrusionDepth extrusionDepth: CGFloat)
  var extrusionDepth: CGFloat
  @NSCopying var string: AnyObject?
  var font: UIFont!
  var isWrapped: Bool
  var containerFrame: CGRect
  var truncationMode: String
  var alignmentMode: String
  var chamferRadius: CGFloat
  @NSCopying var chamferProfile: UIBezierPath?
  @available(iOS 8.0, *)
  var flatness: CGFloat
}
@available(iOS 8.0, *)
enum SCNChamferMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case both
  case front
  case back
}
@available(iOS 8.0, *)
class SCNShape : SCNGeometry {
  convenience init(path path: UIBezierPath?, extrusionDepth extrusionDepth: CGFloat)
  @NSCopying var path: UIBezierPath?
  var extrusionDepth: CGFloat
  var chamferMode: SCNChamferMode
  var chamferRadius: CGFloat
  @NSCopying var chamferProfile: UIBezierPath?
}

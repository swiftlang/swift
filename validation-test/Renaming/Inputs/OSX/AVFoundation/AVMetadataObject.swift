
@available(OSX 10.10, *)
class AVMetadataObject : NSObject {
  var time: CMTime { get }
  var duration: CMTime { get }
  var bounds: CGRect { get }
  var type: String! { get }
}
@available(OSX 10.10, *)
let AVMetadataObjectTypeFace: String
@available(OSX 10.10, *)
class AVMetadataFaceObject : AVMetadataObject, NSCopying {
  var faceID: Int { get }
  var hasRollAngle: Bool { get }
  var rollAngle: CGFloat { get }
  var hasYawAngle: Bool { get }
  var yawAngle: CGFloat { get }
}

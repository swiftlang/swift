
@available(iOS 5.0, *)
class CIFeature : NSObject {
  var type: String { get }
  var bounds: CGRect { get }
}
let CIFeatureTypeFace: String
let CIFeatureTypeRectangle: String
let CIFeatureTypeQRCode: String
let CIFeatureTypeText: String
@available(iOS 5.0, *)
class CIFaceFeature : CIFeature {
  var hasLeftEyePosition: Bool { get }
  var leftEyePosition: CGPoint { get }
  var hasRightEyePosition: Bool { get }
  var rightEyePosition: CGPoint { get }
  var hasMouthPosition: Bool { get }
  var mouthPosition: CGPoint { get }
  var hasTrackingID: Bool { get }
  var trackingID: Int32 { get }
  var hasTrackingFrameCount: Bool { get }
  var trackingFrameCount: Int32 { get }
  var hasFaceAngle: Bool { get }
  var faceAngle: Float { get }
  var hasSmile: Bool { get }
  var leftEyeClosed: Bool { get }
  var rightEyeClosed: Bool { get }
}
@available(iOS 8.0, *)
class CIRectangleFeature : CIFeature {
  var topLeft: CGPoint { get }
  var topRight: CGPoint { get }
  var bottomLeft: CGPoint { get }
  var bottomRight: CGPoint { get }
}
@available(iOS 8.0, *)
class CIQRCodeFeature : CIFeature {
  var topLeft: CGPoint { get }
  var topRight: CGPoint { get }
  var bottomLeft: CGPoint { get }
  var bottomRight: CGPoint { get }
  var messageString: String { get }
}
@available(iOS 9.0, *)
class CITextFeature : CIFeature {
  var topLeft: CGPoint { get }
  var topRight: CGPoint { get }
  var bottomLeft: CGPoint { get }
  var bottomRight: CGPoint { get }
  var subFeatures: [AnyObject] { get }
}

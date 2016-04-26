
@available(iOS 6.0, *)
class AVMetadataObject : NSObject {
  var time: CMTime { get }
  var duration: CMTime { get }
  var bounds: CGRect { get }
  var type: String! { get }
}
@available(iOS 6.0, *)
let AVMetadataObjectTypeFace: String
@available(iOS 6.0, *)
class AVMetadataFaceObject : AVMetadataObject, NSCopying {
  var faceID: Int { get }
  var hasRollAngle: Bool { get }
  var rollAngle: CGFloat { get }
  var hasYawAngle: Bool { get }
  var yawAngle: CGFloat { get }
}
@available(iOS 7.0, *)
let AVMetadataObjectTypeUPCECode: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeCode39Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeCode39Mod43Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeEAN13Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeEAN8Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeCode93Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeCode128Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypePDF417Code: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeQRCode: String
@available(iOS 7.0, *)
let AVMetadataObjectTypeAztecCode: String
@available(iOS 8.0, *)
let AVMetadataObjectTypeInterleaved2of5Code: String
@available(iOS 8.0, *)
let AVMetadataObjectTypeITF14Code: String
@available(iOS 8.0, *)
let AVMetadataObjectTypeDataMatrixCode: String
@available(iOS 7.0, *)
class AVMetadataMachineReadableCodeObject : AVMetadataObject {
  var corners: [AnyObject]! { get }
  var stringValue: String! { get }
}

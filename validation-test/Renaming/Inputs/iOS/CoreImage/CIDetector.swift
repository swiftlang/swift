
@available(iOS 5.0, *)
class CIDetector : NSObject {
  @available(iOS 5.0, *)
  /*not inherited*/ init(ofType type: String, context context: CIContext?, options options: [String : AnyObject]? = [:])
  @available(iOS 5.0, *)
  @discardableResult
  func features(in image: CIImage) -> [CIFeature]
  @available(iOS 5.0, *)
  @discardableResult
  func features(in image: CIImage, options options: [String : AnyObject]? = [:]) -> [CIFeature]
}
@available(iOS 5.0, *)
let CIDetectorTypeFace: String
@available(iOS 8.0, *)
let CIDetectorTypeRectangle: String
@available(iOS 8.0, *)
let CIDetectorTypeQRCode: String
@available(iOS 9.0, *)
let CIDetectorTypeText: String
@available(iOS 5.0, *)
let CIDetectorAccuracy: String
@available(iOS 5.0, *)
let CIDetectorAccuracyLow: String
@available(iOS 5.0, *)
let CIDetectorAccuracyHigh: String
@available(iOS 6.0, *)
let CIDetectorTracking: String
@available(iOS 6.0, *)
let CIDetectorMinFeatureSize: String
@available(iOS 9.0, *)
let CIDetectorNumberOfAngles: String
@available(iOS 5.0, *)
let CIDetectorImageOrientation: String
@available(iOS 7.0, *)
let CIDetectorEyeBlink: String
@available(iOS 7.0, *)
let CIDetectorSmile: String
@available(iOS 8.0, *)
let CIDetectorFocalLength: String
@available(iOS 8.0, *)
let CIDetectorAspectRatio: String
@available(iOS 9.0, *)
let CIDetectorReturnSubFeatures: String

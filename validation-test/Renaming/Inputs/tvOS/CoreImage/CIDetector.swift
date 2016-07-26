
@available(tvOS 5.0, *)
class CIDetector : NSObject {
  @available(tvOS 5.0, *)
  /*not inherited*/ init(ofType type: String, context context: CIContext?, options options: [String : AnyObject]? = [:])
  @available(tvOS 5.0, *)
  @discardableResult
  func features(in image: CIImage) -> [CIFeature]
  @available(tvOS 5.0, *)
  @discardableResult
  func features(in image: CIImage, options options: [String : AnyObject]? = [:]) -> [CIFeature]
}
@available(tvOS 5.0, *)
let CIDetectorTypeFace: String
@available(tvOS 8.0, *)
let CIDetectorTypeRectangle: String
@available(tvOS 8.0, *)
let CIDetectorTypeQRCode: String
@available(tvOS 9.0, *)
let CIDetectorTypeText: String
@available(tvOS 5.0, *)
let CIDetectorAccuracy: String
@available(tvOS 5.0, *)
let CIDetectorAccuracyLow: String
@available(tvOS 5.0, *)
let CIDetectorAccuracyHigh: String
@available(tvOS 6.0, *)
let CIDetectorTracking: String
@available(tvOS 6.0, *)
let CIDetectorMinFeatureSize: String
@available(tvOS 9.0, *)
let CIDetectorNumberOfAngles: String
@available(tvOS 5.0, *)
let CIDetectorImageOrientation: String
@available(tvOS 7.0, *)
let CIDetectorEyeBlink: String
@available(tvOS 7.0, *)
let CIDetectorSmile: String
@available(tvOS 8.0, *)
let CIDetectorFocalLength: String
@available(tvOS 8.0, *)
let CIDetectorAspectRatio: String
@available(tvOS 9.0, *)
let CIDetectorReturnSubFeatures: String

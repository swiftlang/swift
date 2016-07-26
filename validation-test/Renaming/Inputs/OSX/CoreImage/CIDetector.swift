
@available(OSX 10.7, *)
class CIDetector : NSObject {
  @available(OSX 10.7, *)
  /*not inherited*/ init(ofType type: String, context context: CIContext?, options options: [String : AnyObject]? = [:])
  @available(OSX 10.7, *)
  @discardableResult
  func features(in image: CIImage) -> [CIFeature]
  @available(OSX 10.8, *)
  @discardableResult
  func features(in image: CIImage, options options: [String : AnyObject]? = [:]) -> [CIFeature]
}
@available(OSX 10.7, *)
let CIDetectorTypeFace: String
@available(OSX 10.10, *)
let CIDetectorTypeRectangle: String
@available(OSX 10.10, *)
let CIDetectorTypeQRCode: String
@available(OSX 10.11, *)
let CIDetectorTypeText: String
@available(OSX 10.7, *)
let CIDetectorAccuracy: String
@available(OSX 10.7, *)
let CIDetectorAccuracyLow: String
@available(OSX 10.7, *)
let CIDetectorAccuracyHigh: String
@available(OSX 10.8, *)
let CIDetectorTracking: String
@available(OSX 10.8, *)
let CIDetectorMinFeatureSize: String
@available(OSX 10.11, *)
let CIDetectorNumberOfAngles: String
@available(OSX 10.8, *)
let CIDetectorImageOrientation: String
@available(OSX 10.9, *)
let CIDetectorEyeBlink: String
@available(OSX 10.9, *)
let CIDetectorSmile: String
@available(OSX 10.10, *)
let CIDetectorFocalLength: String
@available(OSX 10.10, *)
let CIDetectorAspectRatio: String
@available(OSX 10.11, *)
let CIDetectorReturnSubFeatures: String

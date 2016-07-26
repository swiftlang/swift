
let kCIAttributeFilterName: String
let kCIAttributeFilterDisplayName: String
@available(OSX 10.5, *)
let kCIAttributeDescription: String
@available(OSX 10.11, *)
let kCIAttributeFilterAvailable_Mac: String
@available(OSX 10.11, *)
let kCIAttributeFilterAvailable_iOS: String
@available(OSX 10.5, *)
let kCIAttributeReferenceDocumentation: String
let kCIAttributeFilterCategories: String
let kCIAttributeClass: String
let kCIAttributeType: String
let kCIAttributeMin: String
let kCIAttributeMax: String
let kCIAttributeSliderMin: String
let kCIAttributeSliderMax: String
let kCIAttributeDefault: String
let kCIAttributeIdentity: String
let kCIAttributeName: String
let kCIAttributeDisplayName: String
@available(OSX 10.5, *)
let kCIUIParameterSet: String
@available(OSX 10.5, *)
let kCIUISetBasic: String
@available(OSX 10.5, *)
let kCIUISetIntermediate: String
@available(OSX 10.5, *)
let kCIUISetAdvanced: String
@available(OSX 10.5, *)
let kCIUISetDevelopment: String
let kCIAttributeTypeTime: String
let kCIAttributeTypeScalar: String
let kCIAttributeTypeDistance: String
let kCIAttributeTypeAngle: String
let kCIAttributeTypeBoolean: String
@available(OSX 10.5, *)
let kCIAttributeTypeInteger: String
@available(OSX 10.5, *)
let kCIAttributeTypeCount: String
let kCIAttributeTypePosition: String
let kCIAttributeTypeOffset: String
let kCIAttributeTypePosition3: String
let kCIAttributeTypeRectangle: String
@available(OSX 10.4, *)
let kCIAttributeTypeOpaqueColor: String
@available(OSX 10.11, *)
let kCIAttributeTypeColor: String
@available(OSX 10.4, *)
let kCIAttributeTypeGradient: String
@available(OSX 10.11, *)
let kCIAttributeTypeImage: String
@available(OSX 10.11, *)
let kCIAttributeTypeTransform: String
let kCICategoryDistortionEffect: String
let kCICategoryGeometryAdjustment: String
let kCICategoryCompositeOperation: String
let kCICategoryHalftoneEffect: String
let kCICategoryColorAdjustment: String
let kCICategoryColorEffect: String
let kCICategoryTransition: String
let kCICategoryTileEffect: String
let kCICategoryGenerator: String
@available(OSX 10.5, *)
let kCICategoryReduction: String
let kCICategoryGradient: String
let kCICategoryStylize: String
let kCICategorySharpen: String
let kCICategoryBlur: String
let kCICategoryVideo: String
let kCICategoryStillImage: String
let kCICategoryInterlaced: String
let kCICategoryNonSquarePixels: String
let kCICategoryHighDynamicRange: String
let kCICategoryBuiltIn: String
@available(OSX 10.5, *)
let kCICategoryFilterGenerator: String
@available(OSX 10.4, *)
let kCIApplyOptionExtent: String
@available(OSX 10.4, *)
let kCIApplyOptionDefinition: String
@available(OSX 10.4, *)
let kCIApplyOptionUserInfo: String
@available(OSX 10.4, *)
let kCIApplyOptionColorSpace: String
@available(OSX 10.5, *)
let kCIOutputImageKey: String
@available(OSX 10.5, *)
let kCIInputBackgroundImageKey: String
@available(OSX 10.5, *)
let kCIInputImageKey: String
@available(OSX 10.5, *)
let kCIInputTimeKey: String
@available(OSX 10.5, *)
let kCIInputTransformKey: String
@available(OSX 10.5, *)
let kCIInputScaleKey: String
@available(OSX 10.5, *)
let kCIInputAspectRatioKey: String
@available(OSX 10.5, *)
let kCIInputCenterKey: String
@available(OSX 10.5, *)
let kCIInputRadiusKey: String
@available(OSX 10.5, *)
let kCIInputAngleKey: String
@available(OSX 10.5, *)
let kCIInputRefractionKey: String
@available(OSX 10.5, *)
let kCIInputWidthKey: String
@available(OSX 10.5, *)
let kCIInputSharpnessKey: String
@available(OSX 10.5, *)
let kCIInputIntensityKey: String
@available(OSX 10.5, *)
let kCIInputEVKey: String
@available(OSX 10.5, *)
let kCIInputSaturationKey: String
@available(OSX 10.5, *)
let kCIInputColorKey: String
@available(OSX 10.5, *)
let kCIInputBrightnessKey: String
@available(OSX 10.5, *)
let kCIInputContrastKey: String
@available(OSX 10.5, *)
let kCIInputBiasKey: String
@available(OSX 10.11, *)
let kCIInputWeightsKey: String
@available(OSX 10.5, *)
let kCIInputGradientImageKey: String
@available(OSX 10.5, *)
let kCIInputMaskImageKey: String
@available(OSX 10.5, *)
let kCIInputShadingImageKey: String
@available(OSX 10.5, *)
let kCIInputTargetImageKey: String
@available(OSX 10.5, *)
let kCIInputExtentKey: String
@available(OSX 10.11, *)
let kCIInputVersionKey: String
@available(OSX 10.4, *)
class CIFilter : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.10, *)
  var outputImage: CIImage? { get }
  @available(OSX 10.5, *)
  var name: String
  @available(OSX 10.5, *)
  var isEnabled: Bool
  var inputKeys: [String] { get }
  var outputKeys: [String] { get }
  func setDefaults()
  var attributes: [String : AnyObject] { get }
  @available(OSX 10.4, *)
  @discardableResult
  func apply(_ k: CIKernel, arguments args: [AnyObject]?, options dict: [String : AnyObject]? = [:]) -> CIImage?
}

extension CIFilter {
  func apply(_ k: CIKernel, args args: [AnyObject], options options: (String, AnyObject)...) -> CIImage?
  @available(iOS 8.0, OSX 10.10, *)
  convenience init?(name name: String, elements elements: (String, AnyObject)...)
}
extension CIFilter {
  /*not inherited*/ init?(name name: String)
  @available(OSX 10.10, *)
  /*not inherited*/ init?(name name: String, withInputParameters params: [String : AnyObject]?)
  @discardableResult
  class func filterNames(inCategory category: String?) -> [String]
  @discardableResult
  class func filterNames(inCategories categories: [String]?) -> [String]
  @available(OSX 10.4, *)
  class func registerName(_ name: String, constructor anObject: CIFilterConstructor, classAttributes attributes: [String : AnyObject] = [:])
  @available(OSX 10.4, *)
  @discardableResult
  class func localizedName(forFilterName filterName: String) -> String?
  @available(OSX 10.4, *)
  @discardableResult
  class func localizedName(forCategory category: String) -> String
  @available(OSX 10.4, *)
  @discardableResult
  class func localizedDescription(forFilterName filterName: String) -> String?
  @available(OSX 10.4, *)
  @discardableResult
  class func localizedReferenceDocumentation(forFilterName filterName: String) -> NSURL?
}
extension CIFilter {
  @available(OSX 10.9, *)
  @discardableResult
  class func serializedXMP(from filters: [CIFilter], inputImageExtent extent: CGRect) -> NSData
  @available(OSX 10.9, *)
  @discardableResult
  class func filterArray(fromSerializedXMP xmpData: NSData, inputImageExtent extent: CGRect, error outError: NSErrorPointer) -> [CIFilter]
}

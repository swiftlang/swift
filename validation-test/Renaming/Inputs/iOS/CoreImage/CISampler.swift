
@available(iOS 9.0, *)
class CISampler : NSObject, NSCopying {
  convenience init(image im: CIImage)
  init(image im: CIImage, options dict: [NSObject : AnyObject]? = [:])
  var definition: CIFilterShape { get }
  var extent: CGRect { get }
}
@available(iOS 9.0, *)
let kCISamplerAffineMatrix: String
@available(iOS 9.0, *)
let kCISamplerWrapMode: String
@available(iOS 9.0, *)
let kCISamplerFilterMode: String
@available(iOS 9.0, *)
let kCISamplerWrapBlack: String
@available(iOS 9.0, *)
let kCISamplerWrapClamp: String
@available(iOS 9.0, *)
let kCISamplerFilterNearest: String
@available(iOS 9.0, *)
let kCISamplerFilterLinear: String
@available(iOS 9.0, *)
let kCISamplerColorSpace: String

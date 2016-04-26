
@available(tvOS 9.0, *)
class CISampler : NSObject, NSCopying {
  convenience init(image im: CIImage)
  init(image im: CIImage, options dict: [NSObject : AnyObject]? = [:])
  var definition: CIFilterShape { get }
  var extent: CGRect { get }
}
@available(tvOS 9.0, *)
let kCISamplerAffineMatrix: String
@available(tvOS 9.0, *)
let kCISamplerWrapMode: String
@available(tvOS 9.0, *)
let kCISamplerFilterMode: String
@available(tvOS 9.0, *)
let kCISamplerWrapBlack: String
@available(tvOS 9.0, *)
let kCISamplerWrapClamp: String
@available(tvOS 9.0, *)
let kCISamplerFilterNearest: String
@available(tvOS 9.0, *)
let kCISamplerFilterLinear: String
@available(tvOS 9.0, *)
let kCISamplerColorSpace: String
